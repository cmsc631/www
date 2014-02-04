dummy:
	scribble --htmls ++style extra.css cmsc631.scrbl

push:	
	cp -r papers/ cmsc631/papers/
	scp -r cmsc631/ junkfood.cs.umd.edu:/fs/www/class/spring2014/
