dummy:
	scribble --htmls ++style extra.css cmsc631.scrbl

push:
	scp -r cmsc631/ junkfood.cs.umd.edu:/fs/www/class/spring2014/cmsc631
