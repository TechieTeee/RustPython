use crossbeam_utils::atomic::AtomicCell;
use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use num_traits::{One, Signed, Zero};

use super::int::{PyInt, PyIntRef};
use super::pytype::PyTypeRef;
use super::slice::{PySlice, PySliceRef};
use crate::common::hash::PyHash;
use crate::function::{FuncArgs, OptionalArg};
use crate::slots::{Comparable, Hashable, Iterable, PyComparisonOp, PyIter};
use crate::vm::VirtualMachine;
use crate::{
    iterator, IdProtocol, IntoPyRef, PyClassImpl, PyContext, PyObjectRef, PyRef, PyResult, PyValue,
    TryFromObject, TypeProtocol,
};

// Search flag passed to iter_search
enum SearchType {
    Count,
    Contains,
    Index,
}

// Note: might be a good idea to merge with _membership_iter_search or generalize (_sequence_iter_check?)
// and place in vm.rs for all sequences to be able to use it.
#[inline]
fn iter_search(
    obj: PyObjectRef,
    item: PyObjectRef,
    flag: SearchType,
    vm: &VirtualMachine,
) -> PyResult<usize> {
    let mut count = 0;
    let iter = iterator::get_iter(vm, obj)?;
    while let Some(element) = iterator::get_next_object(vm, &iter)? {
        if vm.bool_eq(&item, &element)? {
            match flag {
                SearchType::Index => return Ok(count),
                SearchType::Contains => return Ok(1),
                SearchType::Count => count += 1,
            }
        }
    }
    match flag {
        SearchType::Count => Ok(count),
        SearchType::Contains => Ok(0),
        SearchType::Index => Err(vm.new_value_error(format!(
            "{} not in range",
            vm.to_repr(&item)
                .map(|v| v.as_str().to_owned())
                .unwrap_or_else(|_| "value".to_owned())
        ))),
    }
}

/// range(stop) -> range object
/// range(start, stop[, step]) -> range object
///
/// Return an object that produces a sequence of integers from start (inclusive)
/// to stop (exclusive) by step.  range(i, j) produces i, i+1, i+2, ..., j-1.
/// start defaults to 0, and stop is omitted!  range(4) produces 0, 1, 2, 3.
/// These are exactly the valid indices for a list of 4 elements.
/// When step is given, it specifies the increment (or decrement).
#[pyclass(module = false, name = "range")]
#[derive(Debug, Clone)]
pub struct PyRange {
    pub start: PyIntRef,
    pub stop: PyIntRef,
    pub step: PyIntRef,
}

impl PyValue for PyRange {
    fn class(vm: &VirtualMachine) -> &PyTypeRef {
        &vm.ctx.types.range_type
    }
}

impl PyRange {
    #[inline]
    fn offset(&self, value: &BigInt) -> Option<BigInt> {
        let start = self.start.as_bigint();
        let stop = self.stop.as_bigint();
        let step = self.step.as_bigint();
        match step.sign() {
            Sign::Plus if value >= start && value < stop => Some(value - start),
            Sign::Minus if value <= self.start.as_bigint() && value > stop => Some(start - value),
            _ => None,
        }
    }

    #[inline]
    pub fn index_of(&self, value: &BigInt) -> Option<BigInt> {
        let step = self.step.as_bigint();
        match self.offset(value) {
            Some(ref offset) if offset.is_multiple_of(step) => Some((offset / step).abs()),
            Some(_) | None => None,
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.length().is_zero()
    }

    #[inline]
    pub fn forward(&self) -> bool {
        self.start.as_bigint() < self.stop.as_bigint()
    }

    #[inline]
    pub fn get(&self, index: &BigInt) -> Option<BigInt> {
        let start = self.start.as_bigint();
        let step = self.step.as_bigint();
        let stop = self.stop.as_bigint();
        if self.is_empty() {
            return None;
        }

        if index.is_negative() {
            let length = self.length();
            let index: BigInt = &length + index;
            if index.is_negative() {
                return None;
            }

            Some(if step.is_one() {
                start + index
            } else {
                start + step * index
            })
        } else {
            let index = if step.is_one() {
                start + index
            } else {
                start + step * index
            };

            if (step.is_positive() && stop > &index) || (step.is_negative() && stop < &index) {
                Some(index)
            } else {
                None
            }
        }
    }

    #[inline]
    fn length(&self) -> BigInt {
        let start = self.start.as_bigint();
        let stop = self.stop.as_bigint();
        let step = self.step.as_bigint();

        match step.sign() {
            Sign::Plus if start < stop => {
                if step.is_one() {
                    stop - start
                } else {
                    (stop - start - 1usize) / step + 1
                }
            }
            Sign::Minus if start > stop => (start - stop - 1usize) / (-step) + 1,
            Sign::Plus | Sign::Minus => BigInt::zero(),
            Sign::NoSign => unreachable!(),
        }
    }
}

// pub fn get_value(obj: &PyObjectRef) -> PyRange {
//     obj.payload::<PyRange>().unwrap().clone()
// }

pub fn init(context: &PyContext) {
    PyRange::extend_class(context, &context.types.range_type);
    PyRangeIterator::extend_class(context, &context.types.range_iterator_type);
}

type PyRangeRef = PyRef<PyRange>;

#[pyimpl(with(Hashable, Comparable, Iterable))]
impl PyRange {
    fn new(cls: PyTypeRef, stop: PyIntRef, vm: &VirtualMachine) -> PyResult<PyRef<Self>> {
        PyRange {
            start: (0).into_pyref(vm),
            stop,
            step: (1).into_pyref(vm),
        }
        .into_ref_with_type(vm, cls)
    }

    fn new_from(
        cls: PyTypeRef,
        start: PyIntRef,
        stop: PyIntRef,
        step: OptionalArg<PyIntRef>,
        vm: &VirtualMachine,
    ) -> PyResult<PyRef<Self>> {
        let step = step.unwrap_or_else(|| (1).into_pyref(vm));
        if step.as_bigint().is_zero() {
            return Err(vm.new_value_error("range() arg 3 must not be zero".to_owned()));
        }
        PyRange { start, stop, step }.into_ref_with_type(vm, cls)
    }

    #[pyproperty(name = "start")]
    fn start(&self) -> PyIntRef {
        self.start.clone()
    }

    #[pyproperty(name = "stop")]
    fn stop(&self) -> PyIntRef {
        self.stop.clone()
    }

    #[pyproperty(name = "step")]
    fn step(&self) -> PyIntRef {
        self.step.clone()
    }

    #[pymethod(name = "__reversed__")]
    fn reversed(&self, vm: &VirtualMachine) -> PyRangeIterator {
        let start = self.start.as_bigint();
        let step = self.step.as_bigint();

        // Use CPython calculation for this:
        let new_stop = start - step;
        let new_start = &new_stop + (self.len() * step);
        let reversed = PyRange {
            start: new_start.into_pyref(vm),
            stop: new_stop.into_pyref(vm),
            step: (-step).into_pyref(vm),
        };

        PyRangeIterator {
            position: AtomicCell::new(0),
            range: reversed.into_ref(vm),
        }
    }

    #[pymethod(name = "__len__")]
    fn len(&self) -> BigInt {
        self.length()
    }

    #[pymethod(name = "__repr__")]
    fn repr(&self) -> String {
        if self.step.as_bigint().is_one() {
            format!("range({}, {})", self.start, self.stop)
        } else {
            format!("range({}, {}, {})", self.start, self.stop, self.step)
        }
    }

    #[pymethod(name = "__bool__")]
    fn bool(&self) -> bool {
        !self.is_empty()
    }

    #[pymethod(name = "__contains__")]
    fn contains(&self, needle: PyObjectRef, vm: &VirtualMachine) -> bool {
        // Only accept ints, not subclasses.
        if let Some(int) = needle.payload_if_exact::<PyInt>(vm) {
            match self.offset(int.as_bigint()) {
                Some(ref offset) => offset.is_multiple_of(self.step.as_bigint()),
                None => false,
            }
        } else {
            iter_search(
                self.clone().into_object(vm),
                needle,
                SearchType::Contains,
                vm,
            )
            .unwrap_or(0)
                != 0
        }
    }

    #[pymethod(name = "__reduce__")]
    fn reduce(&self, vm: &VirtualMachine) -> (PyTypeRef, PyObjectRef) {
        let range_paramters: Vec<PyObjectRef> = vec![&self.start, &self.stop, &self.step]
            .iter()
            .map(|x| x.as_object().clone())
            .collect();
        let range_paramters_tuple = vm.ctx.new_tuple(range_paramters);
        (vm.ctx.types.range_type.clone(), range_paramters_tuple)
    }

    #[pymethod(name = "index")]
    fn index(&self, needle: PyObjectRef, vm: &VirtualMachine) -> PyResult<BigInt> {
        if let Ok(int) = needle.clone().downcast::<PyInt>() {
            match self.index_of(int.as_bigint()) {
                Some(idx) => Ok(idx),
                None => Err(vm.new_value_error(format!("{} is not in range", int))),
            }
        } else {
            // Fallback to iteration.
            Ok(BigInt::from_bytes_be(
                Sign::Plus,
                &iter_search(self.clone().into_object(vm), needle, SearchType::Index, vm)?
                    .to_be_bytes(),
            ))
        }
    }

    #[pymethod(name = "count")]
    fn count(&self, item: PyObjectRef, vm: &VirtualMachine) -> PyResult<usize> {
        if let Ok(int) = item.clone().downcast::<PyInt>() {
            if self.index_of(int.as_bigint()).is_some() {
                Ok(1)
            } else {
                Ok(0)
            }
        } else {
            // Dealing with classes who might compare equal with ints in their
            // __eq__, slow search.
            iter_search(self.clone().into_object(vm), item, SearchType::Count, vm)
        }
    }

    #[pymethod(name = "__getitem__")]
    fn getitem(&self, subscript: RangeIndex, vm: &VirtualMachine) -> PyResult {
        match subscript {
            RangeIndex::Slice(slice) => {
                let (mut substart, mut substop, mut substep) =
                    slice.inner_indices(&self.length(), vm)?;
                let range_step = &self.step;
                let range_start = &self.start;

                substep *= range_step.as_bigint();
                substart = (substart * range_step.as_bigint()) + range_start.as_bigint();
                substop = (substop * range_step.as_bigint()) + range_start.as_bigint();

                Ok(PyRange {
                    start: substart.into_pyref(vm),
                    stop: substop.into_pyref(vm),
                    step: substep.into_pyref(vm),
                }
                .into_ref(vm)
                .into_object())
            }
            RangeIndex::Int(index) => match self.get(index.as_bigint()) {
                Some(value) => Ok(vm.ctx.new_int(value)),
                None => Err(vm.new_index_error("range object index out of range".to_owned())),
            },
        }
    }

    #[pyslot]
    fn tp_new(args: FuncArgs, vm: &VirtualMachine) -> PyResult {
        let range = if args.args.len() <= 2 {
            let (cls, stop) = args.bind(vm)?;
            PyRange::new(cls, stop, vm)
        } else {
            let (cls, start, stop, step) = args.bind(vm)?;
            PyRange::new_from(cls, start, stop, step, vm)
        }?;

        Ok(range.into_object())
    }
}

impl Hashable for PyRange {
    fn hash(zelf: &PyRef<Self>, vm: &VirtualMachine) -> PyResult<PyHash> {
        let length = zelf.length();
        let elements = if length.is_zero() {
            [vm.ctx.new_int(length), vm.ctx.none(), vm.ctx.none()]
        } else if length.is_one() {
            [
                vm.ctx.new_int(length),
                zelf.start().into_object(),
                vm.ctx.none(),
            ]
        } else {
            [
                vm.ctx.new_int(length),
                zelf.start().into_object(),
                zelf.step().into_object(),
            ]
        };
        crate::utils::hash_iter(elements.iter(), vm)
    }
}

impl Comparable for PyRange {
    fn cmp(
        zelf: &PyRef<Self>,
        other: &PyObjectRef,
        op: PyComparisonOp,
        _vm: &VirtualMachine,
    ) -> PyResult<crate::PyComparisonValue> {
        op.eq_only(|| {
            if zelf.is(other) {
                return Ok(true.into());
            }
            let rhs = class_or_notimplemented!(Self, other);
            let lhs_len = zelf.length();
            let eq = if lhs_len != rhs.length() {
                false
            } else if lhs_len.is_zero() {
                true
            } else if zelf.start.as_bigint() != rhs.start.as_bigint() {
                false
            } else if lhs_len.is_one() {
                true
            } else {
                zelf.step.as_bigint() == rhs.step.as_bigint()
            };
            Ok(eq.into())
        })
    }
}

impl Iterable for PyRange {
    fn iter(zelf: PyRef<Self>, vm: &VirtualMachine) -> PyResult {
        Ok(PyRangeIterator {
            position: AtomicCell::new(0),
            range: zelf,
        }
        .into_object(vm))
    }
}

#[pyclass(module = false, name = "range_iterator")]
#[derive(Debug)]
pub struct PyRangeIterator {
    position: AtomicCell<usize>,
    range: PyRangeRef,
}

impl PyValue for PyRangeIterator {
    fn class(vm: &VirtualMachine) -> &PyTypeRef {
        &vm.ctx.types.range_iterator_type
    }
}

#[pyimpl(with(PyIter))]
impl PyRangeIterator {}

impl PyIter for PyRangeIterator {
    fn next(zelf: &PyRef<Self>, vm: &VirtualMachine) -> PyResult {
        let position = BigInt::from(zelf.position.fetch_add(1));
        if let Some(int) = zelf.range.get(&position) {
            Ok(vm.ctx.new_int(int))
        } else {
            Err(vm.new_stop_iteration())
        }
    }
}

pub enum RangeIndex {
    Int(PyIntRef),
    Slice(PySliceRef),
}

impl TryFromObject for RangeIndex {
    fn try_from_object(vm: &VirtualMachine, obj: PyObjectRef) -> PyResult<Self> {
        match_class!(match obj {
            i @ PyInt => Ok(RangeIndex::Int(i)),
            s @ PySlice => Ok(RangeIndex::Slice(s)),
            obj => Err(vm.new_type_error(format!(
                "sequence indices be integers or slices, not '{}'",
                obj.class().name,
            ))),
        })
    }
}
