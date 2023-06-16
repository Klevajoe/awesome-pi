!Author: Wes Kendall
! Copyright 2011 www.mpitutorial.com
program send_rec

implicit none        
 include 'mpif.h'

 integer world_size, world_rank, ierror, numba
 call MPI_INIT(ierror)
 call MPI_COMM_Size(MPI_COMM_WORLD,world_size,ierror)
 call MPI_COMM_Rank(MPI_COMM_WORLD,world_rank,ierror)

! We are assuming at least 2 processes for this task
  if (world_size < 2) then
    print*, "processes for this task must be greater than 1 for world_size =", world_size 
    call MPI_Abort(MPI_COMM_WORLD, 1)
  end if

  if (world_rank == 0) then
    ! If we are rank 0, set the number to -1 and send it to process 1
    numba = -1
    call MPI_Send( numba, 1, MPI_INT, 1, 0, MPI_COMM_WORLD,ierror)
   else if (world_rank == 1) then
    call MPI_Recv(numba, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
    print*, "Process 1 received number ", numba,"from process "
  end if
  call MPI_Finalize(ierror)
end
