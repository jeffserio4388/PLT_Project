import sys
import os

#If user gives a specific set of files from command line
if len(sys.argv)>1:
	testFiles = sys.argv[1:]
else:
	#Get all the files in the tests dir
	testFiles = os.listdir('../basic_tests/')
        #print testFiles
	passTestFiles = [ x for x in testFiles if ( x[-2:]=='pl' and x[:5]=='test-')]
        failTestFiles = [ x for x in testFiles if ( x[-2:]=='pl' and x[:9]=='test-fail')]


#print len(passTestFiles)
#print len(failTestFiles)


#passing and failing
passed = []
failed = []
i=0
print 'Starting the tests..\n'
#Proceed if make succeeds

for file in passTestFiles:
	print file
        stub = (file.split('.'))[0]
	#For each test file perform the test. And print pass or failure
	runStr = './pipeline -d ../basic_tests/' + file

	diff_file = file[:-2]+'diff'
	string = '../basic_tests/'+diff_file
	print string
	#print 'Running for file : '+file +'\n'

	os.system(runStr)

	#print string
	if os.path.exists(string):
		print 'path !! ' + string
		f = open(string)
		s = f.readlines()
		f.close()
		if (len(s) == 0 ):
			passed.append( '**** PASSED when it is supposed to pass for -- '+file+'\n')
		else:
                    failed.append('**** FAILED when it is supposed to pass. For file: '+file+'\n' )
	else :
            failed.append('**** FAILED when it is supposed to pass. For file: '+file+'\n')
	i = i+1
        runStr = 'rm ../basic_tests/' + file[:-3]
        #print(runStr)
        os.system(runStr)

for file in failTestFiles:
	print file
        stub = (file.split('.'))[0]
	#For each test file perform the test. And print pass or failure
	runStr = './pipeline -d ../basic_tests/' + file

	diff_file = file[:-2]+'diff'
	string = '../basic_tests/'+diff_file
	print string
	#print 'Running for file : '+file +'\n'

	os.system(runStr)

	#print string
	if os.path.exists(string):
		print 'path !! ' + string
		f = open(string)
		s = f.readlines()
		f.close()
		if (len(s) == 0 ):
			failed.append( '****FAILED - passes  when it is supposed to fail -- '+file+'\n')
		else:
			failed.append('**** PASS - fails when it is supposed to '+file+'\n' )
	else :
		passed.append('**** PASS - failed it is suposed to fail  '+file+'\n')
	i = i+1
        runStr = 'rm ../basic_tests/' + file[:-3]
        #print(runStr)
        os.system(runStr)
runStr = 'rm ../basic_tests/*.c'
os.system(runStr)

runStr = 'rm *o'
os.system(runStr)
#Printing the results

print '\n---------- PASSED TESTS ------------'
for i in passed:
	print i
print '---------- FAILED TESTS ------------'
for i in failed:
	print i

print '---------- TESTS STATS------------'
print 'Passed : ' + str(len(passed))
print 'Failed : ' + str(len(failed))
