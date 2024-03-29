      subroutine virtual
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for subbasins with multiple HRUs and
!!    prints the daily output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactrolp      |# cfu/m^2     |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# cfu/m^2     |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# cfu/m^2     |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# cfu/m^2     |persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    cbodu(:)      |mg/L          |carbonaceous biological oxygen demand of
!!                                 |surface runoff on current day in HRU
!!    chl_a(:)      |microgram/L   |chlorophyll-a concentration in runoff
!!                                 |on current day in HRU
!!    cklsp(:)      |
!!    curyr         |none          |current year of simulation
!!    da_ha         |ha            |area of watershed in hectares
!!    doxq(:)        |mg/L          |dissolved oxygen concentration in the
!!                                 |surface runoff on current day in HRU
!!    etday         |mm H2O        |actual amount of evapotranspiration that
!!                                 |occurs on day in HRU
!!    gw_q(:)       |mm H2O        |groundwater contribution to streamflow from
!!                                 |HRU on current day
!!    hhqday(:)     |mm H2O        |surface runoff for the hour in HRU
!!    hru_fr(:)     |none          |fraction of watershed area in HRU
!!    hru_fr(:)     |none          |fraction of subbasin area in HRU
!!    hru_ha(:)     |ha            |area of HRU in hectares
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ihout         |none          |hydrograph storage location number for 
!!                                 |subbasin
!!    ihru          |none          |HRU number
!!    inum1         |none          |subbasin number
!!    iprint        |none          |print code:
!!                                 |0 monthly
!!                                 |1 daily
!!                                 |2 annually
!!    irtpest       |none          |the sequence number of the pesticide type
!!                                 |in NPNO(:) which is to be routed through
!!                                 |the watershed
!!    latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!                                 |the day
!!    minpgw(:)     |kg P/ha       |soluble P loading to reach in groundwater
!!    mp            |none          |maximum number of pesticides used in 
!!                                 |watershed
!!    mvaro         |none          |max number of variables routed through the
!!                                 |reach
!!    no3gw(:)      |kg N/ha       |nitrate loading to reach in groundwater
!!    nyskip        |none          |number of years to skip output summarization
!!                                 |and printing
!!    peakr         |m^3/s         |peak runoff rate
!!    pet_day       |mm H2O        |potential evapotranspiration for day in HRU
!!    precipday     |mm H2O        |amount of water reaching soil surface
!!    qday          |mm H2O        |surface runoff loading to main channel for
!!                                 |day in HRU
!!    qdfr          |none          |fraction of water yield that is surface
!!                                 |runoff
!!    qdr(:)        |mm H2O        |total amount of water entering main channel
!!                                 |from HRU for the day
!!    hrupstd(:,1,:)|mg pst        |amount of pesticide type in surface runoff
!!                                 |contribution to stream from HRU on day
!!                                 |(in solution)
!!    hrupstd(:,2,:)|mg pst        |amount of pesticide type in surface runoff
!!                                 |contribution to stream from HRU on day
!!                                 |(sorbed to sediment)
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                                 |sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                                 |sediment in surface runoff in HRU for day
!!    sedorgn(:)    |kg N/ha       |amount of organic nitrogen in surface runoff
!!                                 |in HRU for the day
!!    sedorgp(:)    |kg P/ha       |amount of organic phosphorus in surface
!!                                 |runoff in HRU for the day
!!    sedyld(:)     |metric tons   |daily soil loss caused by water erosion
!!    sepbtm(:)     |mm H2O        |seepage leaving the bottom of the soil
!!                                 |profile on day in HRU
!!    snomlt        |mm H2O        |amount of water in snow melt for the day in
!!                                 |HRU
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(1,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool in first soil
!!                                 |layer of HRU
!!    sol_bd(1,:)   |Mg/m^3        |bulk density of top soil layer in HRU
!!    sol_fon(1,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool in first soil layer
!!                                 |of HRU
!!    sol_fop(1,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool in first soil layer
!!                                 |of HRU
!!    sol_orgn(1,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool in first soil layer of HRU
!!    sol_orgp(1,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool in first soil layer of HRU
!!    sol_pst(:,:,1)|kg/ha         |amount of pesticide in first soil layer in
!!                                 |HRU
!!    sol_stap(1,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus pool
!!    sol_sw(:)     |mm H2O        |amount of water in soil profile in HRU
!!    soxy          |mg/L          |saturation dissolved oxygen concentration
!!    sub_fr(:)     |none          |fraction of watershed area in subbasin
!!    subp(:)       |mm H2O        |precipitation for the day in HRU
!!    surfq(:)      |mm H2O        |amount of water in surface runoff generated
!!                                 |on day in HRU
!!    surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                                 |the day
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    tloss         |mm H2O        |amount of water removed from surface runoff
!!                                 |via transmission losses on day in HRU
!!    tmpav(:)      |deg C         |average air temperature on current day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)     |mm H2O        |amount of water applied to HRU on current
!!                               |day
!!    deepirr(:)  |mm H2O        |amount of water removed from deep aquifer
!!                               |for irrigation
!!    ovrlnd(:)   |mm H2O        |amount of overland flow onto HRU on day
!!    potflwi(:)  |m^3 H2O       |water entering pothole on day
!!    potsedi(:)  |metric tons   |sediment entering pothole on day
!!    shallirr(:) |mm H2O        |amount of water removed from shallow aquifer
!!                               |for irrigation
!!    shyd(1,:)   |m^3 H2O       |water
!!    shyd(2,:)   |metric tons   |sediment or suspended solid load
!!    shyd(3,:)   |kg N          |organic nitrogen
!!    shyd(4,:)   |kg P          |organic phosphorus
!!    shyd(5,:)   |kg N          |nitrate
!!    shyd(6,:)   |kg P          |soluble phosphorus
!!    shyd(7,:)   |kg P          |soluble pesticides
!!    shyd(8,:)   |kg P          |sorbed pesticides
!!    sub_bd(:)   |Mg/m^3        |average bulk density for top 10 mm of soil
!!                               |in subbasin
!!    sub_gwno3(:)|kg N/ha       |nitrate loading in groundwater from subbasin
!!    sub_gwminp(:)|kg N/ha      |soluble P loading in groundwater from subbasin
!!    sub_minp(:) |kg P/ha       |amount of phosphorus stored in all mineral
!!                               |pools sorbed to sediment
!!    submono(1,:)|mm H2O        |precipitation in subbasin for month
!!    submono(2,:)|mm H2O        |snow melt in subbasin for month
!!    submono(3,:)|mm H2O        |surface runoff loading in subbasin for month
!!    submono(4,:)|mm H2O        |water yield from subbasin for month
!!    submono(5,:)|mm H2O        |potential evapotranspiration in subbasin for
!!                               |month
!!    submono(6,:)|mm H2O        |actual evapotranspiration in subbasin for
!!                               |month
!!    submono(7,:)|metric tons/ha|sediment yield from subbasin for month
!!    submono(8,:)|kg N/ha       |organic N loading from subbasin for month
!!    submono(9,:)|kg P/ha       |organic P loading from subbasin for month
!!    submono(10,:)|kg N/ha       |NO3 loading from surface runoff in subbasin
!!                               |for month
!!    submono(11,:)|kg P/ha       |soluble P loading from subbasin for month
!!    submono(12,:)|mm H2O        |groundwater loading from subbasin for month
!!    submono(13,:)|mm H2O        |percolation out of soil profile in subbasin
!!                               |for month
!!    submono(14,:)|kg P/ha       |loading to reach of mineral P attached to
!!                               |sediment from subbasin for month
!!    sub_bactlp(:)|# cfu/m^2     |less persistent bacteria in surface runoff
!!                               |for day in subbasin
!!    sub_bactp(:)|# cfu/m^2     |persistent bacteria in surface runoff for
!!                               |day in subbasin
!!    sub_cbod(:) |kg O2         |carbonaceous biological oxygen demand on day
!!                               |for subbasin
!!    sub_chl(:)  |kg chl-a      |chlorophyll-a in water yield on day in 
!!                               |subbasin
!!    sub_dox(:)  |kg O2         |dissolved oxygen loading on day for subbasin
!!    sub_etday(:)|mm H2O        |actual evapotranspiration on day in subbasin
!!    sub_gwq(:)  |mm H2O        |groundwater flow on day in subbasin
!!    sub_hhsedy(:,:) |metric tons |sediment yield for the time step in subbasin
!!    sub_hhwtmp(:,:) |deg C     |water temperature for the time step in subbasin
!!    sub_latno3(:)|kg N/ha       |NO3-N in lateral flow on day in subbasin
!!    sub_no3(:)  |kg N/ha       |NO3-N in surface runoff on day in subbasin
!!    sub_orgn(:) |kg N/ha       |total organic N in first soil layer of 
!!                               |subbasin
!!    sub_orgp(:) |kg P/ha       |total organic P in first soil layer of
!!                               |subbasin
!!    sub_pet(:)  |mm H2O        |potential evapotranspiration for day in
!!                               |subbasin
!!    sub_precip(:)|mm H2O        |effective precipitation for the day in 
!!                               |subbasin
!!    sub_pst(:,:)|kg pst/ha     |amount of pesticide type in forest soil layer
!!                               |in subbasin
!!    sub_qd(:)   |mm H2O        |surface runoff loading to main channel on
!!                               |day in subbasin
!!    sub_sedpa(:)|kg P/ha       |amount of active mineral P attached to sediment
!!                               |removed in surface runoff on day in subbasin
!!    sub_sedps(:)|kg P/ha       |amount of stable mineral P attached to sediment
!!                               |removed in surface runoff on day in subbasin
!!    sub_sedy(:) |metric tons   |sediment yield for the day in subbasin
!!    sub_sep(:)  |mm H2O        |seepage from bottom of soil profile on day
!!                               |in subbasin
!!    sub_snom(:) |mm H2O        |snow melt on day in subbasin
!!    sub_solp(:) |kg P/ha       |soluble P in surface runoff on day in subbasin
!!    sub_solpst(:)|mg pst        |pesticide in solution in surface runoff on day
!!                               |in subbasin
!!    sub_sorpst(:)|mg pst        |pesticide sorbed to sediment in surface
!!                               |runoff on day in subbasin
!!    sub_subp(:) |mm H2O        |precipitation for day in subbasin
!!    sub_sumfc(:)|mm H2O        |amount of water in soil at field capacity in
!!                               |subbasin
!!    sub_surfq(:)|mm H2O        |surface runoff generated on day in subbasin
!!    sub_sw(:)   |mm H2O        |amount of water in soil in subbasin
!!    sub_tran(:) |mm H2O        |transmission losses for day in subbasin
!!    sub_ubnrunoff(:,:)|mm H2O  |surface runoff from urban impervious cover in subbasin
!!    sub_ubntss(:,:)|metric tons  |TSS loading from urban impervious cover in subbasin
!!    sub_wyld(:) |mm H2O        |water yield on day in subbasin
!!    sub_yorgn(:)|kg N/ha       |organic N in surface runoff on day in subbasin
!!    sub_yorgp(:)|kg P/ha       |organic P in surface runoff on day in subbasin
!!    ubnrunoff(:) |mm H2O      |surface runoff from urban impervious cover 
!!    ubntss(:)    |metric tons    |TSS loading from urban impervious cover
!!    varoute(1,:) |deg C        |temperature
!!    varoute(2,:) |m^3 H2O      |water
!!    varoute(3,:) |metric tons  |sediment or suspended solid load
!!    varoute(4,:) |kg N         |organic nitrogen
!!    varoute(5,:) |kg P         |organic phosphorus
!!    varoute(6,:) |kg N         |nitrate
!!    varoute(7,:) |kg P         |mineral phosphorus
!!    varoute(11,:)|mg pst       |pesticide in solution
!!    varoute(12,:)|mg pst       |pesticide sorbed to sediment
!!    varoute(13,:)|kg           |chlorophyll-a
!!    varoute(16,:)|kg           |carbonaceous biological oxygen demand
!!    varoute(17,:)|kg           |dissolved oxygen
!!    varoute(18,:)|# cfu/100ml  |persistent bacteria
!!    varoute(19,:)|# cfu/100ml  |less persistent bacteria
!!    wcklsp(:)   |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    baseflw     |mm H2O        |difference in total loading and surface runoff
!!                               |loading
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    kk          |none          |counter
!!    sb          |none          |subbasin number
!!    sub_ha      |ha            |area of subbasin in hectares
!!    sub_hwyld(:)|mm H2O        |water yield from subbasin during hour
!!    wtmp        |deg C         |temperature of water
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: hruday, impndday, subday
!!    SWAT: alph, pkq, ysed, enrsb, pesty, orgn, psed
!!    SWAT: Tair

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para   !! Du added........................
      use parm
         use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      integer :: j, sb, kk, ii,ib
      real :: cnv, sub_ha, wtmp, baseflw, bf_fr,hr,ratio
      real :: hqd(4*nstep), hsd(4*nstep), hqdtst(nstep)   ! hqd, hsd locally defined. J.Jeong 4/26/2009      
      real:: den_water=997. !! kg/m3
      !real, dimension(:), allocatable :: tilep           !!R683 1/13/22 nbs     
     

             
      j = ihru
      sb = inum1
      cnv = hru_ha(j) * 10.
!! output for soil moisture content for specific depth for comparation
      call solwout               

!! write daily HRU output
      !|none        |print code:0=monthly,1=daily,2=annual
      !if ((iprint==1.or.iprint==3) .and. curyr > nyskip) call hruday_C   
      !if ((iprint==1.or.iprint==3) .and. curyr > nyskip) call hruday_N   
      !if ((iprint==1.or.iprint==3) .and. curyr > nyskip) call hruday_P    
      !if ((iprint==1.or.iprint==3) .and. curyr > nyskip) call hruday_E  
      if ((iprint==1.or.iprint==3) .and. curyr > nyskip) call hruday
      if ((iprint==1.or.iprint==3) .and. curyr > nyskip) call impndday
     
!! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   sum HRU results for subbasin   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
if (sb > 0) then
      
      !! subbasin averages: water
      sub_subp(sb) = sub_subp(sb) + subp(j) * hru_fr(j)
      sub_snom(sb) = sub_snom(sb) + snomlt * hru_fr(j)
      sub_pet(sb) = sub_pet(sb) + pet_day * hru_fr(j)
      sub_etday(sb) = sub_etday(sb) + etday * hru_fr(j)
      sub_sumfc(sb) = sub_sumfc(sb) + sol_sumfc(j) * hru_fr(j)
      sub_sw(sb) = sub_sw(sb) + sol_sw(j) * hru_fr(j)
      sub_sep(sb) = sub_sep(sb) + sepbtm(j) * hru_fr(j)
      sub_qd(sb) = sub_qd(sb) + qday * hru_fr(j)
      sub_gwq(sb) = sub_gwq(sb) + gw_q(j) * hru_fr(j)
      sub_gwq_d(sb) = sub_gwq_d(sb) + gw_qdeep(j) * hru_fr(j)
      sub_wyld(sb) = sub_wyld(sb) + qdr(j) * hru_fr(j)
      sub_latq(sb) = sub_latq(sb) + latq(j) * hru_fr(j)
      if( ievent_rch==1) then    
            sub_subp_dt(sb,:) = sub_subp_dt(sb,:) + rainsub(j,:) * hru_fr(j)  !!urban modeling by J.Jeong
      end if
      !! subbasin averages: sub-daily water for URBAN MODELING
      if (ievent>0) then
            do ii = 1, nstep  !! step Oct. 18, 2007
              
                if (bmpdrain(j)==1) then
                    !Urban HRUs where runoff drains to bmps
                    sub_ubnrunoff(sb,ii) = sub_ubnrunoff(sb,ii) + (hhqday(ii) + ubnrunoff(ii)) * hru_fr(j) !J.Jeong
		            if (sub_ubnrunoff(sb,ii) < 1.e-20) sub_ubnrunoff(sb,ii) = 0.
                    sub_ubntss(sb,ii) = sub_ubntss(sb,ii)  + (hhsedy(j,ii) + ubntss(ii)) * hru_fr(j) !J.Jeong
		            if (sub_ubntss(sb,ii) < 1.e-20) sub_ubntss(sb,ii) = 0.
                else
                    !Urban/non-urban HRUs that do not make runoff to BMPs
                    sub_hhqd(sb,ii) = sub_hhqd(sb,ii) + (hhqday(ii) + ubnrunoff(ii)) * hru_fr(j)
 		            if (sub_hhqd(sb,ii) < 1.e-20) sub_hhqd(sb,ii) = 0.
                    sub_hhsedy(sb,ii) = sub_hhsedy(sb,ii) + (hhsedy(j,ii) + ubntss(ii)) * hru_fr(j)
   	                if (sub_hhsedy(sb,ii) < 1.e-20) sub_hhsedy(sb,ii) = 0.
                end if 
                !air temperature
                hr = ii * idt / 60. 
                ! sub_atmp(sb,ii) = sub_atmp(sb,ii) + Tair(hr,j) * hru_fr(j)
            end do
      end if

      !! subbasin averages: sediment
      sub_sedy(sb) = sub_sedy(sb) + sedyld(j)
      sub_dsan(sb) = sub_dsan(sb) + sanyld(j)
      sub_dsil(sb) = sub_dsil(sb) + silyld(j)
      sub_dcla(sb) = sub_dcla(sb) + clayld(j)
      sub_dsag(sb) = sub_dsag(sb) + sagyld(j)
      sub_dlag(sb) = sub_dlag(sb) + lagyld(j)

      surqno3(j) = amax1(1.e-12,surqno3(j))
      latno3(j) = amax1(1.e-12,latno3(j))
      no3gw(j) = amax1(1.e-12,no3gw(j))
      surqsolp(j) = amax1(1.e-12,surqsolp(j))
      minpgw(j) = amax1(1.e-12,minpgw(j))
      sedorgn(j) = amax1(1.e-12,sedorgn(j))
      sedorgp(j) = amax1(1.e-12,sedorgp(j))
      sedminpa(j) = amax1(1.e-12,sedminpa(j))
      sedminps(j) = amax1(1.e-12,sedminps(j))
        
      !! subbasin averages: nutrients
      if (latno3(j) < 1.e-6) latno3(j) = 0.0
      sub_no3(sb) = sub_no3(sb) + surqno3(j) * hru_fr(j)
      sub_latno3(sb) = sub_latno3(sb) + latno3(j) * hru_fr(j)
      sub_tileno3(sb) = sub_tileno3(sb) + tileno3(j) * hru_fr(j)
      sub_tilep(sb) = sub_tilep(sb) + tilep(j) * hru_fr(j)       !R683 1/13/22 nbs
 !    sub_tileq(sb) = sub_tileq(sb) + tileq(j) * hru_fr(j)      !! jane f
      sub_tileq(sb) = sub_tileq(sb) + qtile * hru_fr(j)          !! jane f
      sub_vaptile(sb) = sub_vaptile(sb) + vap_tile * hru_fr(j)   !! jane f
      sub_gwno3(sb) = sub_gwno3(sb) + no3gw(j) * hru_fr(j) 
      sub_solp(sb) = sub_solp(sb) + surqsolp(j) * hru_fr(j)
      sub_gwsolp(sb) = sub_gwsolp(sb) + minpgw(j) * hru_fr(j)
      sub_yorgn(sb) = sub_yorgn(sb) + sedorgn(j) * hru_fr(j)
      sub_yorgp(sb) = sub_yorgp(sb) + sedorgp(j) * hru_fr(j)
      sub_sedpa(sb) = sub_sedpa(sb) + sedminpa(j) * hru_fr(j)
      sub_sedps(sb) = sub_sedps(sb) + sedminps(j) * hru_fr(j)
!!-------------------------------------------N/C--------------------------------------------------------------------------
      sub_n2o(sb) = sub_n2o(sb) + N2O(j) *1000 * hru_fr(j)                 !! g/ha day      !!  kg N/ ha d-> g/ha d
      sub_no(sb) = sub_no(sb) + NO(j) *1000 * hru_fr(j)                       !! g/ha day
      sub_nh4(sb) = sub_nh4(sb) + nh4_vol(j) * hru_fr(j)                         !! kg N/ha 
      SUB_CH4g(sb) = SUB_CH4g(sb) + HRU_CH4g (j) * hru_fr(j)
      sub_dnit(sb) = sub_dnit(sb) + no3_denit(j) * hru_fr(j)                      !! kg N/ha 
      sub_nit(sb) = sub_nit(sb) + no3_nitr(j) * hru_fr(j)                            !! kg N/ha 
       
      sub_percno3(sb) = sub_percno3(sb) + no3_perc(j) * hru_fr(j)        !! kg N/ha 
      sub_upno3(sb) = sub_upno3(sb) + no3_up(j) * hru_fr(j)                  !! kg N/ha 
      sub_ferno3(sb) = sub_ferno3(sb) + ( no3_fert(j) + no3_autof(j) )  &   !! kg N/ha 
                                * hru_fr(j)    
      sub_fernh4(sb) = sub_fernh4(sb) + ( nh4_autof(j) + nh4_fert(j) )  &    !! kg N/ha 
                                * hru_fr(j)    
      sub_ferorgn(sb) = sub_ferorgn(sb) +  orgn_fert(j) * hru_fr(j)             !! kg N/ha 
      sub_rainno3(sb) = sub_rainno3(sb) +  no3_rain(j) * hru_fr(j)           !! kg N/ha 
      sub_fixn(sb) = sub_fixn(sb) +  fixn * hru_fr(j)                                   !!  kg N/ha 
      
      sub_solno3(sb) = sub_solno3(sb) +  solc_no3(j)* hru_fr(j)                            !! kg N/ha 
      sub_solnh3(sb) = sub_solnh3(sb) +  solc_nh4(j)* hru_fr(j)                             !! kg N/ha 
      sub_solorgn(sb) = sub_solorgn(sb) + solc_orgn(j)* hru_fr(j)/1000                           !!t N/ha 
      sub_solorgp(sb) = sub_solorgn(sb)  + solc_orgp(j)* hru_fr(j)/1000                            !!t P/ha 
      !-------------C subbasin output------------------------------
      sub_sedc(sb) = sub_sedc(sb) +  sedc_d(j)* hru_fr(j)                        !! kg C/ha
      sub_surfqc(sb) = sub_surfqc(sb) + surfqc_d(j)* hru_fr(j) 
      sub_latc(sb) = sub_latc(sb) + latc_d(j)* hru_fr(j) 
      sub_percc(sb) = sub_percc(sb) + percc_d(j)* hru_fr(j) 
      sub_NPPC(sb) = sub_NPPC(sb) + NPPC_d(j)* hru_fr(j) !! kg C/ha
      sub_rspc(sb) = sub_rspc(sb) + rspc_d(j)* hru_fr(j) 
      sub_solorgc(sb) = sub_solorgc(sb) + solc_orgc(j)* hru_fr(j)/1000.                           !!t c/ha 
      sub_solorgsc(sb) = sub_solorgsc(sb)+solc_orgcs(j)*hru_fr(j)/1000.                            !!t c/ha 
!!-------------------------------------------N/C-------------------------------------

      !!-----------------------------------------------------E------------------------------

      sub_snofall(sb)=sub_snofall(sb)+snofall* hru_fr(j) 
      sub_snodep(sb)=sub_snodep(sb)+sno_dep(j) * hru_fr(j)
      sub_snohru(sb)=sub_snohru(sb)+sno_hru(j) * hru_fr(j)
      sub_sur_tmp(sb)=sub_sur_tmp(sb) + sur_tmp(j) * hru_fr(j)
  
   !  &                         /float(hrutot(sb))
      sub_soltmp_50(sb)=sub_soltmp_50(sb) + soltmp_50(j) * hru_fr(j)
   !  &                         /float(hrutot(sb))
      sub_soltmp_100(sb)=sub_soltmp_100(sb) + soltmp_100(j) * hru_fr(j)
   !  &                         /float(hrutot(sb))
      sub_soltmp_150(sb)=sub_soltmp_150(sb) + soltmp_150(j)  * hru_fr(j)
   !  &                         /float(hrutot(sb))
      sub_soltmp_200(sb)=sub_soltmp_200(sb) + soltmp_200(j) * hru_fr(j)
   !  &                         /float(hrutot(sb))
      sub_soltmp_300(sb)=sub_soltmp_300(sb) + soltmp_300(j)  * hru_fr(j)
   !  &                         /float(hrutot(sb))
      sub_soltmp_500(sb)=sub_soltmp_500(sb) + soltmp_500(j)  * hru_fr(j)
 !    &                         /float(hrutot(sb))
      sub_soltmp_1000(sb)=sub_soltmp_1000(sb)+soltmp_1000(j) * hru_fr(j)
 !    &                         /float(hrutot(sb))
   
      sub_frozday(sb)=sub_frozday(sb)+ sol_frozday(j)* hru_fr(j) 
      sub_airtmp(sb)=sub_airtmp(sb)+ tmpav(j)* hru_fr(j)                                  ! 56 
      
      sub_swc(sb) = sub_swc(sb) +  swc_100(j)* hru_fr(j)   
                                        
   !!---------------------------------Junyu Qi---------------E-------------------------------
      !!!  HRU water temperature contributions ---------------
             
      !! subbasin averages: water temperature
      !! Stefan and Preudhomme. 1993.  Stream temperature estimation
      !! from air temperature.  Water Res. Bull. p. 27-45
      
      if (iwtmp_sub == 0) then     
        wtmp = 0.
        wtmp = 5.0 + 0.75 * tmpav(j)
        sub_wtmp(sb) = sub_wtmp(sb) + wtmp * qdr(j) * hru_fr(j)  
      end if
 
      if (iwtmp_sub == 2 )then    
        ! sub_wtmp_surq(sb)= sub_wtmp_surq(sb)+ wtmp_surq(j)* hru_fr(j)
        ! sub_wtmp_latq(sb)= sub_wtmp_latq(sb)+ wtmp_latq(j)* hru_fr(j)
        ! sub_wtmp_gwq(sb)= sub_wtmp_gwq(sb)+ wtmp_gwq(j)* hru_fr(j)
        ! sub_gwq_deep(sb)= sub_gwq_deep(sb)+gw_qdeep(j)* hru_fr(j) 
        ! sub_wtmp_gwdp(sb)= sub_wtmp_gwdp(sb)+wtmp_gwq(j)*hru_fr(j)  
          sub_wtmp_surq(sb) = sub_wtmp_surq(sb) + wtmp_surq(j) * qday * hru_fr(j)
          sub_wtmp_latq(sb) = sub_wtmp_latq(sb) + wtmp_latq(j) * latq(j) * hru_fr(j)
          sub_wtmp_gwq(sb)  = sub_wtmp_gwq(sb) + wtmp_gwq(j) * gw_q(j) * hru_fr(j)
          sub_wtmp_gwdp(sb) = sub_wtmp_gwdp(sb) + wtmp_gwq(j) * gw_qdeep(j) * hru_fr(j)
      end if
     
     
      !! subbasin averages: pesticides
      if (irtpest > 0) then
          sub_solpst(sb) = sub_solpst(sb) + hrupstd(irtpest,1,j) + hrupstd(irtpest,4,j)
          sub_sorpst(sb) = sub_sorpst(sb) + hrupstd(irtpest,2,j)
      end if

      !! subbasin averages: bacteria
      sub_bactp(sb) = sub_bactp(sb) + (bactrop + bactsedp) * hru_fr(j)
      sub_bactlp(sb) = sub_bactlp(sb) + (bactrolp + bactsedlp) * hru_fr(j)

      !! subbasin averages: water quality indicators
      sub_chl(sb) = sub_chl(sb) + chl_a(j) * (qday * qdfr * cnv) * 1.e-6

      sub_cbod(sb) = sub_cbod(sb) + cbodu(j) * (qdr(j) * qdfr * cnv) * 1.e-3
      sub_dox(sb) = sub_dox(sb) + (doxq(j) * (qdr(j) * qdfr * cnv) +  soxy * (qdr(j) * (1. - qdfr) * cnv)) * 1.e-3
      
      
      !! subbasin averages used in subbasin sediment calculations
      wcklsp(sb) = wcklsp(sb) + cklsp(j) * hru_fr(j)
      sub_precip(sb) = sub_precip(sb) + precipday * hru_fr(j)
      sub_surfq(sb) = sub_surfq(sb) + surfq(j) * hru_fr(j)
      sub_tran(sb) = sub_tran(sb) + tloss * hru_fr(j)
      sub_bd(sb) = sub_bd(sb) + sol_bd(1,j) * hru_fr(j)
	  if (cswat == 0) then
            sub_orgn(sb) = sub_orgn(sb) + (sol_orgn(1,j) + sol_aorgn(1,j) + sol_fon(1,j)) * hru_fr(j)
	  end if
	  if (cswat == 1) then
	    sub_orgn(sb) = sub_orgn(sb) + (sol_n(1,j) + sol_fon(1,j) + sol_mn(1,j)) * hru_fr(j)
	  end if
	  !!add by zhang
	  !!======================
	  if (cswat == 2) then
	    sub_orgn(sb) = sub_orgn(sb) + (sol_LMN(1,j) + sol_LSN(1,j) + sol_HPN(1,j)+sol_HSN(1,j)+sol_BMN(1,j)) * hru_dafr(j)	      
	  end if
	  !!add by zhang
	  !!======================	  	  

      !------------ DOC, POC and DIC---------------------------------------------------!
      if (cswat == 2 ) then
            SUB_RPOC(sb)= SUB_RPOC(sb)+ Sed_RPOC(j)*hru_fr(j)                       !RPOC amount for subbasins (kg/ha)                                          
            SUB_LPOC(sb)= SUB_LPOC(sb)+ Sed_LPOC(j)*hru_fr(j)                        !LPOC amount for subbasins (kg/ha)                                   
            SUB_RDOC(sb)= SUB_RDOC(sb)+ HRU_RDOC(j)*hru_fr(j)                   !RDOC amount for subbasins  (kg/ha)      
            SUB_LDOC(sb)= SUB_LDOC(sb)+ HRU_LDOC(j)*hru_fr(j)                  !LDOC amount for subbasins  (kg/ha)     
            SUB_DIC(sb)= SUB_DIC(sb) + HRU_DIC(j)*hru_fr(j)                             !DIC amount for subbasins (kg/ha)                                
            SUB_CH4s(sb)= SUB_CH4s(sb) + HRU_CH4s(j)* hru_fr(j)
            
            SUB_RPON(sb)= SUB_RPON(sb)+ Sed_RPON(j)*hru_fr(j)                        ! RPON transport  kg N/ha
            SUB_LPON(sb)= SUB_LPON(sb)+ Sed_LPON(j)*hru_fr(j)                       !LPON transport  kg N/ha
            SUB_RDON(sb)= SUB_RDON(sb)+ HRU_RDON(j)*hru_fr(j)       
            SUB_LDON(sb)= SUB_LDON(sb)+ HRU_LDON(j)*hru_fr(j)              
            SUB_DIN(sb)= SUB_DIN(sb)+ HRU_DIN(j)*hru_fr(j) 
            !! N2Os
            !! N2s
            SUB_RPOP(sb)= SUB_RPOP(sb)+ Sed_RPOP(j)*hru_fr(j)                           ! RPOP transport  kg P/ha
            SUB_LPOP(sb)= SUB_LPOP(sb)+ Sed_LPOP(j)*hru_fr(j)                          !LPOP transport  kg P/ha
            SUB_RDOP(sb)= SUB_RDOP(sb)+ HRU_RDOP(j)*hru_fr(j)       
            SUB_LDOP(sb)= SUB_LDOP(sb)+ HRU_LDOP(j)*hru_fr(j)                   
            SUB_DIP(sb)= SUB_DIP(sb)+ HRU_DIP(j)*hru_fr(j) 
      endif
   
   
    !-------------- DOC, POC and DIC-------------------------------------------------!  
end if
!! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$     end subbasin summarization calculations  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!! perform subbasin level operations after processing last HRU in subbasin
      if (iihru == hrutot(sb)) then
        sub_ha = 0.
        sub_ha = da_ha * sub_fr(sb)

        !! calculate subbasin average values for weighted parameters
        if (subfr_nowtr(sb) > 1.e-6) then                   !!non-water HRU fractions
            sub_snom(sb) = sub_snom(sb) / subfr_nowtr(sb)
            sub_sumfc(sb) = sub_sumfc(sb) / subfr_nowtr(sb)
            sub_sw(sb) = sub_sw(sb) / subfr_nowtr(sb)
            sub_sep(sb) = sub_sep(sb) / subfr_nowtr(sb)
            sub_qd(sb) = sub_qd(sb) / subfr_nowtr(sb)
            sub_gwq(sb) = sub_gwq(sb) / subfr_nowtr(sb)
            sub_gwq_d(sb) = sub_gwq_d(sb) / subfr_nowtr(sb)
            sub_wyld(sb) = sub_wyld(sb) / subfr_nowtr(sb)
            sub_latq(sb) = sub_latq(sb) / subfr_nowtr(sb)
            sub_tileq(sb) = sub_tileq(sb) / subfr_nowtr(sb)
        else
            sub_snom(sb) = 0.0
            sub_sumfc(sb) = 0.0
            sub_sw(sb) = 0.0 
            sub_sep(sb) = 0.0
            sub_qd(sb) = 0.0
            sub_gwq(sb) = 0.0
            sub_wyld(sb) = 0.0
            sub_latq(sb) = 0.0
            sub_tileq(sb) = 0.0
        end if  !if (subfr_nowtr(sb) > 1.e-6) then                   !!non-water HRU fractions


        if (ievent > 0) then

            !----------------------------------------------------
            ! Simulate distributed urban BMPs in the subbasin
             call distributed_bmps
            !----------------------------------------------------
              
            !add urban runoff and non-urban runoff
             sub_hhqd(sb,1:nstep) = sub_hhqd(sb,1:nstep) + sub_ubnrunoff(sb,1:nstep)
             sub_hhsedy(sb,1:nstep) = sub_hhsedy(sb,1:nstep) + sub_ubntss(sb,1:nstep)
             
              !route surface runoff in the subbasin
             hqd = 0.; hsd = 0.      !! added on Oct. 22, 2007
             do ii = 1, nstep       
                do ib = 1, itb(sb)
                  hqd(ib+ii-1) = hqd(ib+ii-1) + sub_hhqd(sb,ii) * uh(sb,ib) 
                  hsd(ib+ii-1) = hsd(ib+ii-1) + sub_hhsedy(sb,ii) * uh(sb,ib) 
                end do
             end do
        
             do ii = 1, itb(sb)
                hqd(ii) = hqd(ii) + hqdsave(sb,ii)
                hsd(ii) = hsd(ii) + hsdsave(sb,ii)
             end do

	         sub_hhqd(sb,1:nstep) = max(0.,hqd(1:nstep))
	         sub_hhsedy(sb,1:nstep) = max(0.,hsd(1:nstep))

             do ii = 1, itb(sb)
                hqdsave(sb,ii) = hqd(ii+nstep)  ! save flow after midnight for next day J.Jeong 4/17/2009
                hsdsave(sb,ii) = hsd(ii+nstep)  ! sediment. J.Jeong 4/22/2009
             end do 
         
        end if       !  if (ievent > 0) then


!!---subbasin water temperature----------BEGIN
 
       if ( iwtmp_sub == 0) then   
            !if (sub_wyld(sb) > 0.1) then
            if (sub_wyld(sb) > 1.0e-10) then    
              sub_wtmp(sb) = sub_wtmp(sb) / sub_wyld(sb)
            else
              sub_wtmp(sb) = 0.0
            end if
            
            if (sub_qd(sb)>0.000001) then
                sub_wtmp_surq(sb)= 5.0 + 0.75 * tmpav(hru1(sb))
            else
                sub_wtmp_surq(sb)= 0.
            end if 

            if(sub_latq(sb) > 0.000001) then
             sub_wtmp_latq(sb)= 5.0 + 0.75 * tmpav(hru1(sb)) 
            else 
             sub_wtmp_latq(sb)= 0.
            end if

            if(sub_gwq_d(sb) >0.000001) then
            sub_wtmp_gwq(sb)= 5.0 + 0.75 * tmpav(hru1(sb))
            else
            sub_wtmp_gwq(sb)=0.
            end if
        
            if(wtmp_surq(j)<-1.0) wtmp_surq(j)= -1.0
            if(wtmp_gwq(j)<-1.0) wtmp_gwq(j) = -1.0
            if(wtmp_latq(j) <-1.0) wtmp_latq(j) = -1.0     
   
       end if   !if ( iwtmp_sub == 0) then 
    
       if ( iwtmp_sub == 1 ) call sub_wtmp_Ficklin              !! Ficklin 2012 calculte water temperature of local subbasin by mixing different runoff components
    
       if ( iwtmp_sub == 2 ) call sub_wtmp_Qi                   !! Junyu Qi       

 !!------subbasin water temperature--END

    !! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$     assign reach loadings for subbasin  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         !! zero out hydrograph storage locations
       do ii = 1, mvaro
           varoute(ii,ihout) = 0.
       end do

       !! set values for different routing variables
       !! storage locations set to zero are not currently used
       varoute(1,ihout) = sub_wtmp(sb)                          !!wtmp
       varoute(2,ihout) = sub_wyld(sb) * sub_ha * 10.           !!qdr m3/d m^3 H2O      |water
       varoute(3,ihout) = sub_sedy(sb)                          !!sedyld sub_sedy(:) |metric tons   |sediment yield for the day in subbasin
  
       varoute(4,ihout) = (sub_yorgn(sb) + SUB_RDON(sb) + SUB_LDON(sb))* sub_ha           !!total orgn
       !sub_yorgn(sb) = sub_yorgn(sb) + sedorgn(j) * hru_fr(j)
       !  varoute(4,ihout) = sub_yorgn(sb) * sub_ha                          !!sedorgn
          
       !  varoute(5,ihout) = (sub_yorgp(sb) + sub_sedps(sb) +
       ! &                  sub_sedpa(sb)) * sub_ha          !!sedorgp & sedminps
       varoute(5,ihout) = (sub_yorgp(sb) ) * sub_ha          !!sedorgp & sedminps    
                                           
       varoute(6,ihout) = (sub_no3(sb) + sub_latno3(sb) + sub_tileno3(sb) + sub_gwno3(sb)) * sub_ha          !!surqno3 & latno3 & no3gw
       !varoute(7,ihout) = (sub_solp(sb) + sub_gwsolp(sb)) * sub_ha   !!surqsolp & minpgw & sedminpa   !!R666b 7/19/17 nbs      
       varoute(7,ihout) = (sub_solp(sb) + sub_gwsolp(sb) + sub_tilep(sb)) * sub_ha  !!R683 1/13/22 nbs  !!surqsolp & minpgw & sedminpa
       varoute(8,ihout) = 0.
       varoute(9,ihout) = 0.
       varoute(10,ihout) = 0.
       varoute(11,ihout) = sub_solpst(sb)              !!soluble pst
       varoute(12,ihout) = sub_sorpst(sb)              !!sorb pst
       varoute(13,ihout) = sub_chl(sb)                 !!chl_a
       varoute(14,ihout) = 0.                          !! NH4
       varoute(15,ihout) = 0.                          !! NO2
       varoute(16,ihout) = sub_cbod(sb)                !!cbodu
       varoute(17,ihout) = sub_dox(sb)                 !!doxq & soxy
       if (varoute(2,ihout) > .1) then
            varoute(18,ihout) = sub_bactp(sb) * sub_ha / varoute(2,ihout) !cfu/100ml
            varoute(19,ihout) = sub_bactlp(sb) * sub_ha / varoute(2,ihout)
       end if
       varoute(20,ihout) = 0.                            !! cmetal #1
       varoute(21,ihout) = 0.                            !! cmetal #2
       varoute(22,ihout) = 0.                            !! cmetal #3
       varoute(23,ihout) = sub_dsan(sb)                  !! detached sand
       varoute(24,ihout) = sub_dsil(sb)                  !! detached silt
       varoute(25,ihout) = sub_dcla(sb)                  !! detached clay
       varoute(26,ihout) = sub_dsag(sb)                  !! detached sml ag
       varoute(27,ihout) = sub_dlag(sb)                  !! detached lrg ag 
       varoute(29,ihout) = sub_qd(sb) * sub_ha * 10.     !! surface runoff
       varoute(30,ihout) = sub_latq(sb) * sub_ha * 10.   !! lateral flow
       varoute(31,ihout) = sub_tileq(sb) * sub_ha * 10.  !! tile flow
       varoute(32,ihout) = sub_gwq(sb) * sub_ha * 10.    !! groundwater flow 
       !! varoute array has space for 33 different routing components
       
       !------------DOC and POC for routing----------------------!
       if (cswat == 2 ) then
           varoute(33,ihout)    = SUB_RPOC(sb) * sub_ha   !RPOC amount in subbasin for routing (kg)
           !Sed_RPOC(j) = (sol_HSC(1,j)+sol_HPC(1,j))*(1.0- X1) 
           varoute(34,ihout)    = SUB_LPOC(sb) * sub_ha   !LPOC amount in subbasin for routing (kg)
           !orgncswat2-Zhang-Du-Qi.f90(85):      Sed_LPOC(j) = (sol_LMC(1,j)+sol_LSC(1,j)+sol_BMC(1,j))*(1.0-X1) 
           varoute(35,ihout)    = SUB_RDOC(sb) * sub_ha   !RDOC amount in subbasin for routing (kg)
           !subbasin-Qi.f90(524):        HRU_RDOC(j) = SurQ_DOC(j)+ LatQT_DOC(j)+  GwQ_DOC(j)
           varoute(36,ihout)    = SUB_LDOC(sb) * sub_ha   !LDOC amount in subbasin for routing (kg)
           !orgncswat2-Zhang-Du-Qi.f90(526):       HRU_LDOC(j)=  0.
           varoute(37,ihout)    = SUB_DIC(sb) * sub_ha    !DIC amount  in subbasin for routing (kg) 
           ! HRU_DIC(j) = SurQ_DIC(j)+ LatQT_DIC(j)  + GwQ_DIC(j)
           varoute(38,ihout)    = SUB_CH4s(sb) * sub_ha   !  
           !sim_initday-Qi.f90(464):      HRU_CH4s=0.; not used
           varoute(39,ihout)    = SUB_RPON(sb) * sub_ha    !RPON amount in subbasin for routing (kg)
           !Sed_RPON(j) =(sol_HSN(1,j)+sol_HPN(1,j))*(1.0- xx1)
           varoute(40,ihout)    = SUB_LPON(sb) * sub_ha    !LPON amount in subbasin for routing (kg)
           !orgncswat2-Zhang-Du-Qi.f90(184):       Sed_LPON(j)=(sol_LMN(1,j)+sol_LSN(1,j)+sol_BMN(1,j))*(1.0-xx1)
           varoute(41,ihout)    = SUB_RDON(sb) * sub_ha    !RDON amount in subbasin for routing (kg)
           !sim_initday-Qi.f90(495):      HRU_RDON = 0.; not used
           varoute(42,ihout)    = SUB_LDON(sb) * sub_ha    !LDON amount in subbasin for routing (kg)
           !sim_initday-Qi.f90(496):      HRU_LDON = 0.; not used
           varoute(43,ihout)    = SUB_DIN(sb) * sub_ha   !DIN amount in subbasin for routing (kg)
           !SUB_DIN(sb)= SUB_DIN(sb)+ HRU_DIN(j)*hru_fr(j) 
           !HRU_DIN(j) = surqno3(j) + latno3(j)+ no3gw(j)+ tileno3(j)
           varoute(44,ihout)= SUB_N2Os(sb) *sub_ha  !not used; N2O amount in subbasin for routing (kg)
           varoute(45,ihout)= SUB_N2s(sb) *sub_ha   !not used
           
           varoute(46,ihout)=SUB_RPOP(sb)*sub_ha    !RPOP amount in subbasin for routing (kg)
           !psed-Qi.f90(231):        Sed_RPOP(j) = sedorgp(j) * (sol_orgp(1,j) / porgg)
           varoute(47,ihout)=SUB_LPOP(sb)*sub_ha    !LPOP amount in subbasin for routing (kg)
           !Sed_LPOP(j) = sedorgp(j) * (sol_fop(1,j) / porgg)
           varoute(48,ihout)=SUB_RDOP(sb)*sub_ha    !RDOP amount in subbasin for routing (kg)
           !sim_initday-Qi.f90(502):      HRU_RDOP= 0.; not used
           varoute(49,ihout)=SUB_LDOP(sb)*sub_ha    !LDOP amount in subbasin for routing (kg)
           !sim_initday-Qi.f90(503):      HRU_LDOP = 0.; not used
           varoute(50,ihout)=SUB_DIP(sb) *sub_ha    ! DIP amount in subbasin for routing (kg)
           !subbasin-Qi.f90(528):        HRU_DIP(j) = surqsolp(j)+minpgw(j)+sedminpa(j)+sedminps(j)

       endif    !if (cswat == 2 ) then
      !-----------DOC and POC for routing------------------------!


      !! sum variables for hyd.out
      do ii = 1, 6
         shyd(ii,ihout) = shyd(ii,ihout) + varoute(ii+1,ihout)
      end do
      shyd(7,ihout) = shyd(7,ihout) + varoute(11,ihout)
      shyd(8,ihout) = shyd(8,ihout) + varoute(12,ihout)
         
    !!===============subdaily loading from subbasins to stream============================================================��
      !! sub-daily calculations
      if (ievent_rch > 0) then           
          !! determine the daily total base flow 
          !baseflw = sub_gwq(sb) + sub_latq(sb) + sub_tileq(sb)
          !if (baseflw < 0.) baseflw = 0.
         
          !Urban modeling by J.Jeong 4/23/2008
          !Daily water yield is unevenly distributed over time 
          !based on fractional rainfall of the day
          !sub_hwyld = 0.
             
          do ii = 1, nstep_rch
         !   if(baseflw>0.1 .and. sum(precipdt)>0.1) then
             ! bf_fr = bf_flg * precipdt(ii+1) / sum(precipdt) +
   !  &    !     (1. - bf_flg) * 1. / nstep_rch     
            !  sub_hwyld(sb,ii) = sub_hhqd(sb,ii) + baseflw * bf_fr
           ! else
            !  sub_hwyld(sb,ii) = sub_hhqd(sb,ii) + baseflw / nstep_rch
           ! endif            
             sub_hwyld(sb,ii) = sub_wyld(sb) / real(nstep_rch)                
            !! water temperature, equation 2.3.13 in SWAT manual
           ! sub_hhwtmp(sb,ii) = 5.0 + 0.75 * sub_atmp(sb,ii)
             sub_hhwtmp(sb,ii) =  sub_wtmp(sb)                                     
          end do

          !! assign reach loadings for subbasin
          !! zero out hydrograph storage locations
          do ii = 1, mvaro
            do kk = 1, nstep_rch
                hhvaroute(ii,ihout,kk) = 0.
            end do
          end do

          !! set values for different routing variables
          !! storage locations set to zero are not currently used
          do ii = 1, nstep_rch
            ratio = 0.
            !if (sub_wyld(sb) > 1.e-3)    
            if (sub_wyld(sb) > 0.)  ratio = sub_hwyld(sb,ii) / sub_wyld(sb)   	                      
                               
            if (sub_hwyld(sb,ii) > 0.) then
              hhvaroute(1,ihout,ii) = sub_hhwtmp(sb,ii)                 !!wtmp
              hhvaroute(2,ihout,ii) = sub_hwyld(sb,ii) * sub_ha * 10.   !!water   
              hhvaroute(3,ihout,ii) = varoute(3,ihout) * ratio          !!sub_hhsedy(sb,ii)            !!sedyld
              hhvaroute(4,ihout,ii) = varoute(4,ihout) * ratio          !!sedorgn
              hhvaroute(5,ihout,ii) = varoute(5,ihout) * ratio          !!sedorgp
              hhvaroute(6,ihout,ii) = varoute(6,ihout) * ratio          !!no3
              hhvaroute(7,ihout,ii) = varoute(7,ihout) * ratio          !!minp
              hhvaroute(8,ihout,ii) = 0.
              hhvaroute(9,ihout,ii) = 0.
              hhvaroute(10,ihout,ii) = 0.
              hhvaroute(11,ihout,ii) = varoute(11,ihout) * ratio        !!sol pst
              hhvaroute(12,ihout,ii) = varoute(12,ihout) * ratio        !!sorb pst
              hhvaroute(13,ihout,ii) = varoute(13,ihout) * ratio        !!chl_a
              hhvaroute(14,ihout,ii) = 0.                               !!NH3
              hhvaroute(15,ihout,ii) = 0.                               !!NO2
              hhvaroute(16,ihout,ii) = varoute(16,ihout) * ratio        !!cbodu
              hhvaroute(17,ihout,ii) = varoute(17,ihout) * ratio        !!doxq & soxy
              hhvaroute(18,ihout,ii) = varoute(18,ihout)                !!bactp
              hhvaroute(19,ihout,ii) = varoute(19,ihout)                !!bactlp
              hhvaroute(20,ihout,ii) = 0.                               !!cmetal#1
              hhvaroute(21,ihout,ii) = 0.                               !!cmetal#2
              hhvaroute(22,ihout,ii) = 0.                               !!cmetal#3
              
              QHY(ii,ihout,IHX(1))=hhvaroute(2,ihout,ii)/(dthy * 3600.) !m3 -> m3/s Jaehak flood routing 2017 !!R666b 7/19/17 nbs 
            
              
            if (cswat == 2 ) then
                hhvaroute(33,ihout,ii) = varoute(33,ihout)* ratio    !RPOC amount in subbasin for routing (kg)
                hhvaroute(34,ihout,ii) = varoute(34,ihout)* ratio    !LPOC amount in subbasin for routing (kg)
                hhvaroute(35,ihout,ii) = varoute(35,ihout)* ratio    !RDOC amount in subbasin for routing (kg)
                hhvaroute(36,ihout,ii) = varoute(36,ihout)* ratio    !LDOC amount in subbasin for routing (kg)
                hhvaroute(37,ihout,ii) = varoute(37,ihout)* ratio    !DIC amount  in subbasin for routing (kg) 
                hhvaroute(38,ihout,ii) = varoute(38,ihout)* ratio    !CH4 solu amount in subbasin for routing (kg)
                
                hhvaroute(39,ihout,ii) = varoute(39,ihout)* ratio    !LPON amount in subbasin for routing (kg)
                hhvaroute(40,ihout,ii) = varoute(40,ihout)* ratio    !RDON amount in subbasin for routing (kg)
                hhvaroute(41,ihout,ii) = varoute(41,ihout)* ratio    !LDON amount in subbasin for routing (kg)
                hhvaroute(42,ihout,ii) = varoute(42,ihout)* ratio    !LDON amount in subbasin for routing (kg)
                hhvaroute(43,ihout,ii) = varoute(43,ihout)* ratio    !DIN (kg)
                hhvaroute(44,ihout,ii) = varoute(44,ihout)* ratio    !N2Os (kg)
                hhvaroute(45,ihout,ii) = varoute(45,ihout)* ratio    !N2s (kg)
                
                hhvaroute(46,ihout,ii) = varoute(46,ihout)* ratio    !LPOP amount in subbasin for routing (kg)
                hhvaroute(47,ihout,ii) = varoute(47,ihout)* ratio    !RDOP amount in subbasin for routing (kg)
                hhvaroute(48,ihout,ii) = varoute(48,ihout)* ratio    !LDOP amount in subbasin for routing (kg)
                hhvaroute(49,ihout,ii) = varoute(49,ihout)* ratio    !LDOP amount in subbasin for routing (kg)
                hhvaroute(50,ihout,ii) = varoute(50,ihout)* ratio    !DIP (kg)
           endif
              
       
            end if
                
		  end do
        end if         !!if (ievent_rch > 0) then        
 
  !!=============subdaily loading from subbasins to stream==============================================================��
  
 
  
        !! summary calculations
        if (curyr > nyskip) then
             submono(1,sb) = submono(1,sb) + sub_subp(sb)
             submono(2,sb) = submono(2,sb) + sub_snom(sb)
             submono(3,sb) = submono(3,sb) + sub_qd(sb)
             submono(4,sb) = submono(4,sb) + sub_wyld(sb)
             submono(5,sb) = submono(5,sb) + sub_pet(sb)
             submono(6,sb) = submono(6,sb) + sub_etday(sb)
             submono(7,sb) = submono(7,sb) + sub_sedy(sb) / sub_ha
             submono(8,sb) = submono(8,sb) + sub_yorgn(sb)
             submono(9,sb) = submono(9,sb) + sub_yorgp(sb)
             submono(10,sb) = submono(10,sb) + sub_no3(sb)
             submono(11,sb) = submono(11,sb) + sub_solp(sb)
             submono(12,sb) = submono(12,sb) + sub_gwq(sb)
             submono(13,sb) = submono(13,sb) + sub_sep(sb)
             submono(14,sb) = submono(14,sb) + sub_sedpa(sb) + sub_sedps(sb)
             submono(15,sb) = submono(15,sb) + sub_latq(sb)
             submono(16,sb) = submono(16,sb) + sub_latno3(sb)
             submono(17,sb) = submono(17,sb) + sub_gwno3(sb) 
             submono(18,sb) = submono(18,sb) + sub_tileq(sb)    !! jane f. 
             submono(19,sb) = submono(19,sb) + sub_tileno3(sb)
             submono(20,sb) = submono(20,sb) + sub_vaptile(sb)  !! jane f.
             
        
           !!-------------------------------------N/C---------------------------------------------------------
             submono(21,sb) = submono(21,sb) + sub_n2o(sb)        !! g/ha day
             submono(22,sb) = submono(22,sb) + sub_no(sb)           !! g/ha day
             submono(23,sb) = submono(23,sb) + sub_nh4(sb)        !!kg N/ha 
             submono(24,sb) = submono(24,sb) + sub_ch4(sb) 
             submono(25,sb) = submono(25,sb) + sub_dnit(sb)             !!kg N/ha 
             submono(26,sb) = submono(26,sb) +  sub_nit(sb)               !!kg N/ha 
             submono(27,sb) = submono(27,sb) +  sub_percno3(sb)     !!kg N/ha 
             submono(28,sb) = submono(28,sb) +  sub_upno3(sb)
             submono(29,sb) = submono(29,sb) +  sub_ferno3(sb)              !!kg N/ha 
             submono(30,sb) = submono(30,sb) +  sub_fernh4(sb)               !!kg N/ha 
             submono(31,sb) = submono(31,sb) +  sub_ferorgn(sb)     !!kg N/ha 
             submono(32,sb) = submono(32,sb) +  sub_rainno3(sb)  
             submono(33,sb) = submono(33,sb) +  sub_fixn(sb)               !!kg N/ha 
             submono(34,sb) = submono(34,sb) +  sub_solno3(sb)                !!kg N/ha 
             submono(35,sb) = submono(35,sb) +  sub_solnh3(sb)               !!kg N/ha 
             submono(36,sb) = submono(36,sb) +  sub_solorgn(sb)                !!kg N/ha 
             submono(37,sb) = submono(37,sb) +  sub_solorgp(sb)              !!kg N/ha 
             submono(38,sb) = submono(38,sb) +  sub_sedc(sb) 
             submono(39,sb) = submono(39,sb) +  sub_surfqc(sb) 
             submono(40,sb) = submono(40,sb) +  sub_latc(sb)
             submono(41,sb) = submono(41,sb) +  sub_percc(sb) 
             submono(42,sb) = submono(42,sb) +  sub_NPPC(sb) 
             submono(43,sb) = submono(43,sb) +  sub_rspc(sb) 
!!--------------------------------------------N/C-------------------------------------

!!---------------------------------------------------E------------------------------
            submono(44,sb) = submono(44,sb) + sub_snofall(sb)
            submono(45,sb) = submono(45,sb) +  sub_snodep(sb)
            submono(46,sb) = submono(46,sb) +  sub_snohru(sb)
            submono(47,sb) = submono(47,sb) +  sub_sur_tmp(sb)
            submono(48,sb) = submono(48,sb) +  sub_soltmp_50(sb)
            submono(49,sb) = submono(49,sb) +  sub_soltmp_100(sb)
            submono(50,sb) = submono(50,sb) +  sub_soltmp_150(sb)
            submono(51,sb) = submono(51,sb) +  sub_soltmp_200(sb)
            submono(52,sb) = submono(52,sb) +  sub_soltmp_300(sb)
            submono(53,sb) = submono(53,sb) +  sub_soltmp_500(sb)
            submono(54,sb) = submono(54,sb) +  sub_soltmp_1000(sb)
            submono(55,sb) = submono(55,sb) +  sub_frozday(sb)
            submono(56,sb) = submono(56,sb) +  sub_airtmp(sb)
            submono(57,sb) = submono(57,sb) +  sub_swc(sb) 
    !!-------------------------------------E-------------------------------------------------------
            submono(58,sb) = submono(58,sb) + sub_solorgc(sb)
            submono(59,sb) = submono(59,sb) + sub_solorgsc(sb)
             
            submono(60,sb) = submono(60,sb) +  sub_wtmp_surq(sb)
            submono(61,sb) = submono(61,sb) + sub_wtmp_latq(sb)
            submono(62,sb) = submono(62,sb) + sub_wtmp_gwq(sb)

         if (iprint == 1) call subday      

          
        end if    ! if (curyr > nyskip) then
      endif     ! if (iihru == hrutot(sb)) then

!! initialize irrigation/overland flow variables
!! these variable must be initialized here because irrigation/overland
!! flow is performed in different command loops and the water used in
!! irrigation will not be reported in output files if the 
!! variables are initialized in the regular place

!! if rch or res sources reset aird here

!!!! Srin's irrigation source by each application changes
!!      irrsc(j) = irr_sc(nro(j),nirr(j),j)
!!!! Srin's irrigation source by each application changes

!!	if (irrsc(j) <= 2)  aird(j) = 0.

      shallirr(j) = 0.
      deepirr(j) = 0.
      ovrlnd(j) = 0.
      potflwi(j) = 0.
      potsedi(j) = 0.
      potsani(j) = 0.
      potsili(j) = 0.
      potclai(j) = 0.
      potsagi(j) = 0.
      potlagi(j) = 0.
!! end of day calculations
      tmpavp(j) = 0.
      tmpavp(j) = tmpav(j)
      
      return   
      end