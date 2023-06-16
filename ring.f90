!Author: Wes Kendall
! Copyright 2011 www.mpitutorial.com
program ring


 implicit none        
 include 'mpif.h'

 integer world_size, world_rank, ierror, token
 call MPI_INIT(ierror)
 call MPI_COMM_Size(MPI_COMM_WORLD,world_size,ierror)
 call MPI_COMM_Rank(MPI_COMM_WORLD,world_rank,ierror)

 ! Receive from the lower process and send to the higher process. Take care
  ! of the special case when you are the first process to prevent deadlock.



  if (world_rank /= 0) then
    call MPI_Recv(token, 1, MPI_INT, world_rank - 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
    print*, "Process", world_rank, " received token ", token,  "from process ", (world_rank - 1)
  else
    ! Set the token's value if you are process 0
    token = -1
  end if
  call MPI_Send(token, 1, MPI_INT, MODULO((world_rank + 1) , world_size), 0, MPI_COMM_WORLD,ierror)
  ! Now process 0 can receive from the last process. This makes sure that at
  ! least one MPI_Send is initialized before all MPI_Recvs (again, to prevent
  ! deadlock)
  if (world_rank == 0) then
    call MPI_Recv(token, 1, MPI_INT, world_size - 1, 0, MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror);
    print*, "Process", world_rank, " received token ", token,  "from process ", (world_rank - 1)
  end if
 call MPI_Finalize(ierror)
end
