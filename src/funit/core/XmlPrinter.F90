#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: XmlPrinter
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Halvor Lund, SINTEF Energy Research
!!
!! @date
!! 30 Jan 2014
!!
!! @note <A note here.>
!! Need to improve the handling of nested quotes.
!
! REVISION HISTORY:
! 2014 June 4 ML Rilee
!    Added intermediate status output. Refactored prints to handle both single
!    and arrays of Failure and Success.  Exceptions can be printed too. Quotes 
!    are not handled well: need to consider going to "&quot;" and "&apos;".
!    May need to separate status reports from the end-of-run summary
!
!-------------------------------------------------------------------------------
module PF_XmlPrinter
   use PF_Exception
   use PF_AbstractPrinter
   implicit none
   private

   public :: XmlPrinter

   type, extends(AbstractPrinter) :: XmlPrinter
      integer :: unit
      integer :: privateUnit
   contains
      procedure :: addFailure
      procedure :: addError
      procedure :: startTest
      procedure :: endTest
      procedure :: endRun
      procedure :: print
      procedure :: printHeader
      procedure :: printTestsuiteHeader
      procedure :: printFailure
      procedure :: printFailures
      procedure :: printExceptions
      procedure :: printSuccess
      procedure :: printSuccesses
      procedure :: printFooter
      procedure :: addSuccess
      procedure :: extractSuiteNames
      procedure :: printSuites
      procedure :: printOneSuite
   end type XmlPrinter

   interface XmlPrinter
      module procedure new_XmlPrinter_unit
   end interface

contains

   function new_XmlPrinter_unit(unit) result(printer)
      type (XmlPrinter) :: printer
      integer, intent(in) :: unit

      printer%unit = unit

    end function new_XmlPrinter_unit

   subroutine addFailure(this, testName, exceptions)
      use PF_ExceptionList
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
      _UNUSED_DUMMY(exceptions)
   end subroutine addFailure

   subroutine addError(this, testName, exceptions)
      use PF_ExceptionList
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
      _UNUSED_DUMMY(exceptions)
   end subroutine addError

   subroutine startTest(this, testName)
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
   end subroutine startTest

   subroutine endTest(this, testName)
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
   end subroutine endTest

   subroutine endRun(this, result, elapsed_time)
     use PF_AbstractTestResult, only : AbstractTestResult
     class (XmlPrinter), intent(inOut) :: this
     class (AbstractTestResult), intent(in) :: result
      real, intent(in) :: elapsed_time

     call this%print(result, elapsed_time)

   end subroutine endRun

   subroutine print(this, result, elapsed_time)
      use PF_AbstractTestResult, only : AbstractTestResult
      use PF_TestFailureVector
      use PF_TestFailure
      class(XmlPrinter), intent(in) :: this
      class(AbstractTestResult), intent(in) :: result
      real, intent(in) :: elapsed_time

      type(TestFailureVector) :: successes, errors, failures
      character(:), dimension(:), allocatable :: suiteNames
      integer :: numSuites

      _UNUSED_DUMMY(elapsed_time)

      ! Get all test results
      successes = result%getSuccesses()
      errors = result%getErrors()
      failures = result%getFailures()

      ! Extract unique suite names from all tests
      call this%extractSuiteNames(successes, errors, failures, suiteNames, numSuites)

      call this%printHeader(result)
      call this%printSuites(suiteNames, numSuites, successes, errors, failures)
      call this%printFooter(result)

   end subroutine print

   subroutine printHeader(this, result)
      use PF_AbstractTestResult, only : AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      _UNUSED_DUMMY(result)

      write(this%unit,'(a)') '<?xml version="1.0" encoding="UTF-8"?>'
      write(this%unit,'(a)') '<testsuites>'
      flush(this%unit)

   end subroutine printHeader

   subroutine printTestsuiteHeader(this, suiteName, numTests, numErrors, numFailures, totalTime)
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: suiteName
      integer, intent(in) :: numTests, numErrors, numFailures
      real, intent(in) :: totalTime

      write(this%unit,'(a,a,a,i0,a,i0,a,i0,a,f0.4,a)') &
           '<testsuite name="', cleanXml(suiteName), &
           '" errors="', numErrors, &
           '" failures="', numFailures, &
           '" tests="', numTests, &
           '" time="', totalTime, '">'
      flush(this%unit)

   end subroutine printTestsuiteHeader

   subroutine printFailure(this, label, aFailedTest)
      use PF_TestFailure
      use PF_SourceLocation
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: aFailedTest

      call this%printExceptions(label,aFailedTest%testName,&
           aFailedTest%exceptions, aFailedTest%time)

   end subroutine printFailure

   subroutine printExceptions(this, label, testName, exceptions, test_time)
      use PF_TestFailure
      use PF_SourceLocation
      use PF_ExceptionList
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: testName
      type(ExceptionList), intent(in) :: exceptions
      real, intent(in) :: test_time

      class(Exception), pointer  :: pException
      character(:), allocatable :: methodName

      integer :: j
      character(len=80) :: locationString

      methodName = getMethodName(testName)

      ! Write testcase opening tag
      write(this%unit,'(a,a,a,f0.4,a)') '<testcase name="', &
           cleanXml(trim(methodName)), '" time="', test_time, '">'

      ! Write failure/error elements
      do j= 1, exceptions%size()
         pException => exceptions%at(j)
         locationString = pException%location%toString()

         write(this%unit,'(a,a,a)',advance='no') '<', cleanXml(label), ' message="'
         write(this%unit,'(a,a,a)',advance='no') &
              'Location: ', cleanXml(trim(locationString)), ', '
         write(this%unit,'(a)',advance='no') &
              cleanXml(trim(pException%getMessage()))
         write(this%unit,*) '"/>'
      end do
      write(this%unit,'(a)') '</testcase>'

      flush(this%unit)

   end subroutine printExceptions


!mlr old version
   subroutine printFailure1(this, label, aFailedTest)
      use PF_TestFailure
      use PF_SourceLocation
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: aFailedTest
      class (Exception), pointer :: pException

      integer :: j
      character(len=80) :: locationString

!mlr testcase should likely be testname or testmethod or maybe test
!mlr Q?  What does JUnit do?
!mlr  Ask Halvor -- good for 3.0
      write(this%unit,'(a,a,a)') '<testcase name="', &
           cleanXml(trim(aFailedTest%testName)), '">'
      do j= 1, aFailedTest%exceptions%size()
        pException => aFailedTest%exceptions%at(j)
        locationString = pException%location%toString()

        write(this%unit,'(a,a,a)',advance='no') &
             '<', cleanXml(label), ' message="'
        write(this%unit,'(a,a,a)',advance='no') &
             'Location: ', cleanXml(trim(locationString)), ', '
        write(this%unit,'(a)',advance='no') &
             cleanXml(trim(pException%getMessage()))
        write(this%unit,*) '"/>'
      end do
      write(this%unit,'(a)') '</testcase>'

      flush(this%unit)

   end subroutine printFailure1

   subroutine printFailures(this, label, failures, suiteName)
      use PF_TestFailure
      use PF_TestFailureVector
      use PF_SourceLocation
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailureVector), intent(in) :: failures
      character(len=*), intent(in), optional :: suiteName

      integer :: i
      type(TestFailure) :: aTest

      do i = 1, failures%size()
         aTest = failures%at(i)
         if (present(suiteName)) then
            if (trim(getClassname(aTest%testName)) == trim(suiteName)) then
               call this%printFailure(label, aTest)
            end if
         else
            call this%printFailure(label, aTest)
         end if
      end do

   end subroutine printFailures

   subroutine printTestName(this, testName)
      use PF_TestFailure
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: testName

      write(this%unit,'(a,a,a)') '<testcase name="',&
           cleanXml(trim(testName)), '"/>'

      flush(this%unit)

    end subroutine printTestName

   subroutine printSuccess(this, aSuccessTest)
      use PF_TestFailure
      class (XmlPrinter), intent(in) :: this
      type (TestFailure) :: aSuccessTest
      character(:), allocatable :: methodName

      methodName = getMethodName(aSuccessTest%testName)

      write(this%unit,'(a,a,a,f0.4,a)') '<testcase name="',&
           cleanXml(trim(methodName)), '" time="', aSuccessTest%time, '"/>'

      flush(this%unit)

   end subroutine printSuccess

   subroutine printSuccesses(this, successes, suiteName)
      use PF_TestFailure
      use PF_TestFailurevector
      class (XmlPrinter), intent(in) :: this
      type (TestFailureVector), intent(in) :: successes
      character(len=*), intent(in), optional :: suiteName

      integer :: i
      type(TestFailure) :: aTest

      do i = 1, successes%size()
         aTest = successes%at(i)
         if (present(suiteName)) then
            if (trim(getClassname(aTest%testName)) == trim(suiteName)) then
               call this%printSuccess(aTest)
            end if
         else
            call this%printSuccess(aTest)
         end if
      end do

   end subroutine printSuccesses

   subroutine printFooter(this, result)
      use PF_AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      _UNUSED_DUMMY(result)

      write(this%unit,'(a)') '</testsuites>'
      flush(this%unit)

   end subroutine printFooter

   ! Extract unique suite names from all test results
   subroutine extractSuiteNames(this, successes, errors, failures, suiteNames, numSuites)
      use PF_TestFailureVector
      use PF_TestFailure
      class(XmlPrinter), intent(in) :: this
      type(TestFailureVector), intent(in) :: successes, errors, failures
      character(:), dimension(:), allocatable, intent(out) :: suiteNames
      integer, intent(out) :: numSuites

      character(len=80), dimension(:), allocatable :: tempNames
      integer :: totalTests

      _UNUSED_DUMMY(this)

      totalTests = successes%size() + errors%size() + failures%size()
      if (totalTests == 0) then
         numSuites = 0
         return
      end if

      allocate(tempNames(totalTests))
      numSuites = 0

      ! Extract suite names from all test result types
      call addSuiteNamesFromVector(successes, tempNames, numSuites)
      call addSuiteNamesFromVector(errors, tempNames, numSuites)
      call addSuiteNamesFromVector(failures, tempNames, numSuites)

      ! Copy to output array
      if (numSuites > 0) then
         allocate(character(len=80) :: suiteNames(numSuites))
         suiteNames(1:numSuites) = tempNames(1:numSuites)
      end if

      deallocate(tempNames)

   contains

      subroutine addSuiteNamesFromVector(testVector, uniqueNames, count)
         type(TestFailureVector), intent(in) :: testVector
         character(len=80), dimension(:), intent(inout) :: uniqueNames
         integer, intent(inout) :: count

         character(len=80) :: suiteName
         integer :: i, j
         logical :: found
         type(TestFailure) :: aTest

         do i = 1, testVector%size()
            aTest = testVector%at(i)
            suiteName = getClassname(aTest%testName)
            if (len_trim(suiteName) > 0) then
               found = .false.
               do j = 1, count
                  if (trim(uniqueNames(j)) == trim(suiteName)) then
                     found = .true.
                     exit
                  end if
               end do
               if (.not. found) then
                  count = count + 1
                  uniqueNames(count) = suiteName
               end if
            end if
         end do

      end subroutine addSuiteNamesFromVector

   end subroutine extractSuiteNames

   ! Print multiple testsuite elements
   subroutine printSuites(this, suiteNames, numSuites, successes, errors, failures)
      use PF_TestFailureVector
      use PF_TestFailure
      class(XmlPrinter), intent(in) :: this
      character(len=*), dimension(:), intent(in) :: suiteNames
      integer, intent(in) :: numSuites
      type(TestFailureVector), intent(in) :: successes, errors, failures

      integer :: i

      do i = 1, numSuites
         call this%printOneSuite(trim(suiteNames(i)), successes, errors, failures)
      end do

   end subroutine printSuites

   ! Print one testsuite element for a specific suite
   subroutine printOneSuite(this, suiteName, successes, errors, failures)
      use PF_TestFailureVector
      use PF_TestFailure
      class(XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: suiteName
      type(TestFailureVector), intent(in) :: successes, errors, failures

      integer :: i, suiteTests, suiteErrors, suiteFailures
      real :: suiteTime
      type(TestFailure) :: aTest

      ! Count tests, errors, and failures for this suite
      suiteTests = 0
      suiteErrors = 0
      suiteFailures = 0
      suiteTime = 0.0

      do i = 1, successes%size()
         aTest = successes%at(i)
         if (trim(getClassname(aTest%testName)) == trim(suiteName)) then
            suiteTests = suiteTests + 1
            suiteTime = suiteTime + aTest%time
         end if
      end do

      do i = 1, errors%size()
         aTest = errors%at(i)
         if (trim(getClassname(aTest%testName)) == trim(suiteName)) then
            suiteTests = suiteTests + 1
            suiteErrors = suiteErrors + 1
            suiteTime = suiteTime + aTest%time
         end if
      end do

      do i = 1, failures%size()
         aTest = failures%at(i)
         if (trim(getClassname(aTest%testName)) == trim(suiteName)) then
            suiteTests = suiteTests + 1
            suiteFailures = suiteFailures + 1
            suiteTime = suiteTime + aTest%time
         end if
      end do

      call this%printTestsuiteHeader(suiteName, suiteTests, suiteErrors, suiteFailures, suiteTime)

      call this%printSuccesses(successes, suiteName)
      call this%printFailures('error', errors, suiteName)
      call this%printFailures('failure', failures, suiteName)

      write(this%unit,'(a)') '</testsuite>'
      flush(this%unit)

   end subroutine printOneSuite

   ! Helper function to extract classname from test name
   ! Test names are formatted as "suite.testmethod"
   function getClassname(testName) result(classname)
      character(len=*), intent(in) :: testName
      character(:), allocatable :: classname
      integer :: dot_pos

      dot_pos = index(testName, '.', back=.true.)
      if (dot_pos > 0) then
         classname = testName(1:dot_pos-1)
      else
         classname = ''
      end if
   end function getClassname

   ! Helper function to extract test method name from full test name
   function getMethodName(testName) result(methodName)
      character(len=*), intent(in) :: testName
      character(:), allocatable :: methodName
      integer :: dot_pos

      dot_pos = index(testName, '.', back=.true.)
      if (dot_pos > 0) then
         methodName = testName(dot_pos+1:)
      else
         methodName = testName
      end if
   end function getMethodName

   function cleanXml(string_in) result(out)
      character(len=*), intent(in) :: string_in
      character(:), allocatable :: out

      out = string_in
      out = replaceAll(out, '<', '[')
      out = replaceAll(out, '>', ']')
      out = replaceAll(out, '"', "'")
   end function cleanXml

   function replaceAll(string_in, search, replace) result(out)
      character(len=*), intent(in) :: string_in
      character, intent(in) :: search, replace
      character(:), allocatable :: out
      integer :: i
      out = string_in
      i = index(out, search)
      do while(i /= 0)
         out = out(:i-1) // replace // out(i+1:)
         i = index(out, search)
      end do
   end function replaceAll

   subroutine addSuccess(this, testName)
      class (XmlPrinter), intent(inout) :: this
      character(*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)

   end subroutine addSuccess

end module PF_XmlPrinter
