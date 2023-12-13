      subroutine soltly
      !!This subroutine is used to add extra soil layers for soil temperature module
      use parm
      use parm_subE
      implicit none
      
      integer :: j, k, nly, nlyr
      real :: f,dp,ww, wc,b,d
       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j)
       nly1=0
       
      ! calculate maximum damping depth
      !! SWAT manual equation 2.3.6
      f = 0.
      dp = 0.
      f = sol_avbd(j) / (sol_avbd(j) + 686. * Exp(-5.63 * sol_avbd(j)))              !sol_avbd     |Mg/m^3        |average bulk density for soil profile
      dp = 1000. + 2500. * f

      !! calculate scaling factor for soil water
      !! SWAT manual equation 2.3.7
      ww = 0.
      wc = 0.
      ww = .356 - .144 * sol_avbd(j)
      wc = sol_sw(j) / (ww * sol_z(sol_nly(j),j))

      !! calculate daily value for damping depth
      !! SWAT manual equation 2.3.8
      b = 0.
      f = 0.
      d = 0.
      b = Log(500. / dp)
      f = Exp(b * ((1. - wc) / (1. + wc))**2)
      d = f * dp
      ddepth(j)=d     !! damping depth  mm
      ddepth(j)=dp
      
      !! nlyr~(0-10)   added layers
      nlyr=5                                                                  
      
	  if (ddepth(j)> sol_z(sol_nly(j),j) )then   !! if damping depth is > soil depth
            nly1 = sol_nly(j)+ nlyr
            do k=1, nlyr
                sol_thic(sol_nly(j)+k,j) = (ddepth(j) - sol_z(sol_nly(j),j))/nlyr
                k_sol(sol_nly(j)+k,j) = k_sol(sol_nly(j),j)*1        !! assign heat conductivity value
	            ca_sol(sol_nly(j)+k,j) = ca_sol(sol_nly(j),j)         !! assign heat capacity value
	        end do
 
          !! depth to point k from soil surface mm
           sol_cd(1+nly,j) = sol_z(nly,j) + sol_thic(1+nly,j)/2     
           sol_cd(2+nly,j) = sol_cd(1+nly,j) + sol_thic(1+nly,j)
           sol_cd(3+nly,j) = sol_cd(2+nly,j) + sol_thic(1+nly,j)
           sol_cd(4+nly,j) = sol_cd(3+nly,j) + sol_thic(1+nly,j)
           sol_cd(5+nly,j) = sol_cd(4+nly,j) + sol_thic(1+nly,j)

	 else     !! if damping depth is < soil depth

	     do k=nly,1,-1
	        if ( ddepth(j)>sol_z(k-1,j) .and. ddepth(j)<=sol_z(k,j) )then
                nly1=k
                exit
	        end if
	     end do

	end if


    return
	end
