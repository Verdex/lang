

program : a -> b;

bind : m a -> ( a -> m b ) -> m b;
bind = \ a -> \ gen -> gen ( unwrap a );

blah = (\ a -> \ b -> a) A B;

s = (\ a -> \ b -> a); 

j = (\ a -> \ b -> a) (\ a -> a);
