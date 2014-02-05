dummy:
	scribble --htmls ++style extra.css cmsc631.scrbl

push:	
	cp -r papers/ cmsc631/papers/
	rsync -avz cmsc631 junkfood.cs.umd.edu:/fs/www/class/spring2014/cmsc631
