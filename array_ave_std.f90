program array_ave

! Author: Wes Kendall
! Copyright 2013 www.mpitutorial.com
 implicit none        
 include 'mpif.h'

 integer world_size, world_rank, ierror, i,j,num_elements_per_proc
 integer, parameter :: num_elements=10
 real rand_nums(num_elements)
 real local_sum, global_sum
 
 !CHARACTER(100) :: num1char
 !CHARACTER(100) :: num2char
 !REAL :: num1
 !REAL :: num2
 !REAL :: numsum
 
 local_sum=0

!First, make sure the right number of inputs have been provided
 !if (COMMAND_ARGUMENT_COUNT() /= 1) then
 ! write(*,*)'ERROR, ONE COMMAND-LINE ARGUMENTS REQUIRED, STOPPING'
 ! stop
 !end if

 !CALL GET_COMMAND_ARGUMENT(1,num1char)   !first, read in the two values
 !CALL GET_COMMAND_ARGUMENT(2,num2char)

 !READ(num1char,*)num_elements                    !then, convert them to REALs
 !READ(num2char,*)num2
 

 
   ! Calculate the number of iterations for each rank
 num_elements_per_proc = num_elements/world_rank
 if (MODULO(num_elements, world_rank) > 0) then
        ! add 1 in case the number of ranks doesn't divide the number of numbers
    num_elements_per_proc = num_elements_per_proc + 1
 end if


 
 call MPI_INIT(ierror)
 call MPI_COMM_Size(MPI_COMM_WORLD,world_size,ierror)
 call MPI_COMM_Rank(MPI_COMM_WORLD,world_rank,ierror)

 

 
! Create a random array of elements on all processes.

  do i = 1, num_elements
   rand_nums(i) = i!rand()
  end do
  ! Sum the numbers locally

  do i = 1, num_elements_per_proc
    local_sum = local_sum + rand_nums(i)
  end do

!Print the random numbers on each process
  print*, "Local sum for process", world_rank, '-', local_sum, 'avg =', (local_sum / num_elements_per_proc)

 ! Reduce all of the local sums into the global sum
 
  call MPI_Reduce(local_sum, global_sum, 1, MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD, ierror)

! Print the result
  if (world_rank == 0) then
    print*, "Total sum = ", global_sum, "avg =", (global_sum / (world_size * num_elements_per_proc))
   end if

 ! Clean up
  !free(rand_nums);

  call MPI_Barrier(MPI_COMM_WORLD);
  call MPI_Finalize(ierror);





end
