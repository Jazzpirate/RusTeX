use std::marker::PhantomData;

trait AT<T:AllT<A=Self>> {}
trait BT<T:AllT<B=Self>> {}
trait CT<T:AllT<C=Self>> {}
trait DT<T:AllT<D=Self>> {}

trait AllT: Sized {
    type A:AT<Self>;
    type B:BT<Self>;
    type C:CT<Self>;
    type D:DT<Self>;
}

struct TypeA<A:AT<Self>,B:BT<Self>,C:CT<Self>,D:DT<Self>>(PhantomData<A>,PhantomData<B>,PhantomData<C>,PhantomData<D>);
impl<A:AT<Self>,B:BT<Self>,C:CT<Self>,D:DT<Self>> AllT for TypeA<A,B,C,D> {
    type A = A;
    type B = B;
    type C = C;
    type D = D;
}
