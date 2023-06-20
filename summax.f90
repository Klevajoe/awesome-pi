program sum_and_max

  implicit none
  include "mpif.h"

  integer rank, n_ranks, ierr
  
  integer, parameter :: n_numbers=10
  real vector(n_numbers)
  real vsum, vmax, my_first_number
  integer i

  ! First call MPI_Init
  call MPI_Init(ierr)

  ! Get my rank and the number of ranks
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  ! Each rank will have n_numbers numbers,
  ! starting from where the previous left
  my_first_number = n_numbers*rank;

  ! Set the vector
  do i = 1, n_numbers
     vector(i) = rand()
  end do


  ! Find the sum and print
  call find_sum( vector, n_numbers, vsum )
  write(6,*) "Sum = ", vsum

  ! Find the maximum and print
  call find_max( vector, n_numbers, vmax )
  write(6,*) "Maximum = ", vmax


  ! Call MPI_Finalize at the end
  call MPI_Finalize(ierr)

contains

  ! Calculate the sum of numbers in a vector
  subroutine find_sum( vector, N, global_sum )
     implicit none
     include "mpif.h"

     real, intent(in) :: vector(:)
     real, intent(inout) :: global_sum
     real vsum
     integer, intent(in) :: N
     integer i, ierr
    
     vsum = 0
     do i = 1, N
        vsum = vsum + vector(i)
     end do

     ! Call MPI_Allreduce to find the full sum
     call MPI_Allreduce( vsum, global_sum, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr )

  end subroutine find_sum


  ! Find the maximum of numbers in a vector
  subroutine find_max( vector, N, global_max )
     implicit none
     include "mpif.h"

     real, intent(in) :: vector(:)
     real, intent(inout) :: global_max
     real vmax
     integer, intent(in) :: N
     integer i, ierr
    
     vmax = 0
     do i = 1, N
        if (vmax < vector(i)) then
           vmax = vector(i)
        end if
     end do

     ! Call MPI_Allreduce to find the full maximum
     call MPI_Allreduce( vmax, global_max, 1, MPI_REAL, MPI_MAX, MPI_COMM_WORLD, ierr )

  end subroutine find_max
end
