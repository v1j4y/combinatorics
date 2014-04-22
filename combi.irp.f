program combi
    implicit none
    integer,allocatable::comb(:)
    integer::m,n
    integer::i,j,count

    read(5,*)m,n
    write(6,*)m,n

    allocate (comb(n))

    count=1

    do i=1,n
        comb(i)=i
    enddo
    write(6,*)(comb(j),j=1,n)

!C main loop
    do while(comb(1) .lt. m-n+1)

    i=n
    do while(comb(i).eq.m-(n-i))
        i-=1
    enddo
    
    comb(i)+=1
    do j=i+1,n
        comb(j)=comb(i)+(j-i)
    enddo
    write(6,*)(comb(j),j=1,n)
    count+=1

    enddo
    print *,count
end
