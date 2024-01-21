all: 
	clean; \
	make -C ../; \
	erlc *.erl; \

clean: 
	rm *.beam *.dump; \
	ls | grep -v -E 'sendReceive.erl' | grep erl | xargs rm -rf