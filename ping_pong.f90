!Author: Wes Kendall
! Copyright 2011 www.mpitutorial.com
program ping
         
 implicit none        
 include 'mpif.h'

 integer world_size, world_rank, ierror, ping_pong_count, partner_rank, PING_PONG_LIMIT
 PING_PONG_LIMIT = 10
 call MPI_INIT(ierror)
 call MPI_COMM_Size(MPI_COMM_WORLD,world_size,ierror)
 call MPI_COMM_Rank(MPI_COMM_WORLD,world_rank,ierror)

 ! We are assuming 2 processes for this task
 if (world_size /= 2) then
    print *,  "World size must be two not", world_size
    call MPI_Abort(MPI_COMM_WORLD, 1);
  end if

 ping_pong_count = 0



 partner_rank =MODULO((world_rank + 1) , 2)
 Do  while (ping_pong_count < PING_PONG_LIMIT) 
    if (world_rank == MODULO( ping_pong_count , 2)) then
        ! Increment the ping pong count before you send it
        ping_pong_count = ping_pong_count + 1
        call MPI_Send(ping_pong_count, 1, MPI_INT, partner_rank, 0, MPI_COMM_WORLD,ierror)
        print *, world_rank, " sent and incremented ping_pong_count", ping_pong_count, "_to", partner_rank
     else 
        call MPI_Recv(ping_pong_count, 1, MPI_INT, partner_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        print *, world_rank,  "received ping_pong_count",  ping_pong_count, "_to", partner_rank
     end if
 end do


 call MPI_FINALIZE(ierror);

end
