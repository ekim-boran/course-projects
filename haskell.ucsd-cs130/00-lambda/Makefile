test:
	stack test --allow-different-user

bin:
	stack build --allow-different-user

clean:
	stack clean --allow-different-user

turnin: 
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream https://github.com/ucsd-cse130/00-lambda.git

update:
	git pull upstream master
