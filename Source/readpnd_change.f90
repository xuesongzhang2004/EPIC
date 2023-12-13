      subroutine readpnd

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads data from the HRU/subbasin pond input file (.pnd).
!!    This file contains data related to ponds and wetlands in the 
!!    HRUs/subbasins.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hrutot(:)   |none          |number of HRUs in subbasin
!!    i           |none          |subbasin number
!!    ihru        |none          |HRU number
!!    nhru        |none          |number of last HRU in previous subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    chlap(:)    |none          |chlorophyll-a production coefficient for pond
!!    chlaw(:)    |none          |chlorophyll-a production coefficient for 
!!                               |wetland
!!    iflod1(:)   |none          |beginning month of non-flood season
!!    iflod2(:)   |none          |ending month of non-flood season
!!    ipnd1(:)    |none          |beginning month of nutrient settling season
!!    ipnd2(:)    |none          |ending month of nutrient settling season
!!    ndtarg(:)   |none          |number of days required to reach target 
!!                               |storage from current pond storage
!!    nsetlp(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlp(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    nsetlw(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlw(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    pnd_esa(:)  |ha            |surface area of ponds when filled to
!!                               |emergency spillway
!!    pnd_evol(:) |10**4 m**3 H2O|runoff volume from catchment area needed
!!                               |to fill the ponds to the emergency spillway
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into ponds
!!    pnd_k(:)    |mm/hr         |hydraulic conductivity through bottom of 
!!                               |ponds
!!    pnd_no3(:)  |kg N          |amount of nitrate in pond 
!!    pnd_nsed(:) |mg/L          |normal sediment concentration in pond water
!!    pnd_orgn(:) |kg N          |amount of organic N in pond
!!    pnd_orgp(:) |kg P          |amount of organic P in pond
!!    pnd_psa(:)  |ha            |surface area of ponds when filled to 
!!                               |principal spillway
!!    pnd_pvol(:) |10**4 m**3 H2O|runoff volume from catchment area needed to 
!!                               |fill the ponds to the principal spillway
!!    pnd_sed(:)  |mg/L          |sediment concentration in pond water
!!    pnd_solp(:) |kg P          |amount of soluble P in pond
!!    pnd_vol(:)  |10**4 m**3 H2O|volume of water in ponds
!!    psetlp(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlp(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    psetlw(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlw(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    seccip(:)   |none          |water clarity coefficient for pond
!!    secciw(:)   |none          |water clarity coefficient for wetland
!!    wet_fr(:)   |none          |fraction of HRU/subbasin area that drains 
!!                               |into wetlands
!!    wet_k(:)    |mm/hr         |hydraulic conductivity of bottom of wetlands
!!    wet_mxsa(:) |ha            |surface area of wetlands at maximum water 
!!                               |level
!!    wet_mxvol(:)|10**4 m**3 H2O|runoff volume from catchment area needed to
!!                               |fill wetlands to maximum water level
!!    wet_no3(:)  |kg N          |amount of nitrate in wetland
!!    wet_nsa(:)  |ha            |surface area of wetlands in subbasin at
!!                               |normal water level
!!    wet_nsed(:) |mg/L          |normal sediment concentration in wetland 
!!                               |water
!!    wet_nvol(:) |10**4 m**3 H2O|runoff volume from catchment area needed to 
!!                               |fill wetlands to normal water level
!!    wet_orgn(:) |kg N          |amount of organic N in wetland
!!    wet_orgp(:) |kg P          |amount of organic P in wetland
!!    wet_sed(:)  |mg/L          |sediment concentration in wetland water
!!    wet_solp(:) |kg P          |amount of soluble P in wetland
!!    wet_vol(:)  |10**4 m**3 H2O|volume of water in wetlands
!!    dtp_onoff(:)   |none          |sub-basin detention pond is associated with
!!    dtp_iy(:)      |none          |year of the simulation that the reservoir 
!!                                  |becomes operational
!!    dtp_imo(:)     |none          |month the reservoir becomes operational
!!    dtp_evrsv      |none          |detention pond evaporation coefficient
!!    dtp_numweir(:) |none          |Total number of weirs in the BMP
!!    dtp_numstage(:)|none          |Total number of stages in the weir
!!    dtp_lwratio(:)   |none          |Ratio of length to width of water back up
!!    dtp_totwrwid(:)|m             |Total constructed width of the detention wall across
!!                                  |the creek
!!    dtp_stagdis(:) |none          |0=use weir/orifice discharge equation to calculate 
!!                                  |outflow, 1=use stage-dicharge relationship
!!    dtp_reltype(:) |none          |Equations for Stage-Discharge relationship,1=exponential 
!!                                  |function, 2=linear, 3=logarithmic, 4=cubic, 5=power   
!!    dtp_intcept(:) |none          |Intercept used in regression equations
!!    dtp_expont(:)  |none          |Exponent used in the exponential equation
!!    dtp_coef1(:)   |none          |Coefficient of 3rd degree in the polynomial equation
!!    dtp_coef2(:)   |none          |Coefficient of 2nd degree in the polynomial equation
!!    dtp_coef3(:)   |none          |Coefficient of 1st degree in the polynomial equation
!!    dtp_dummy1(:)   |none         |Dummy variable, backs up space
!!    dtp_dummy2(:)   |none         |Dummy variable, backs up space
!!    dtp_dummy3(:)   |none         |Dummy variable, backs up space
!!    dtp_weirtype(:,:)|none        |Type of weir: 1=rectangular and 2=circular
!!    dtp_weirdim(:,:)|none         |Weir dimensions, 1=read user input, 0=use model calculation
!!    dtp_wdratio(:,:)|none         |Width depth ratio of rectangular weirs
!!    dtp_depweir(:,:)|m            |Depth of rectangular wier at different stages
!!    dtp_diaweir(:,:)|m            |Diameter of orifice hole at different stages
!!    dtp_addon(:,:)  |m            |The distance between spillway levels
!!    dtp_flowrate(:,:)|m3/sec      |Maximum discharge from each stage of the weir/hole
!!    dtp_cdis(:,:)  |none          |Discharge coeffieicne for weir/orifice flow
!!    dtp_retperd(:,:)|years        |Return period at different stages
!!    dtp_pcpret(:,:)|mm            |precipitation for different return periods (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    eof         |none          |end of file flag
!!    evpnd       |none          |pond evaporation coefficient
!!    j           |none          |counter
!!    schla       |none          |value for CHLA used in subbasin
!!    schlaw      |none          |value for CHLAW used in subbasin
!!    sifld1      |none          |value for IFLOD1 used in subbasin
!!    sifld2      |none          |value for IFLOD2 used in subbasin
!!    sn1         |m/year        |value for NSETL1 used in subbasin
!!    sn2         |m/year        |value for NSETL2 used in subbasin
!!    sndt        |none          |value for NDTARG used in subbasin
!!    snw1        |m/year        |value for NSETLW1 used in subbasin
!!    snw2        |m/year        |value for NSETLW2 used in subbasin
!!    sp1         |m/year        |value for PSETL1 used in subbasin
!!    sp2         |m/year        |value for PSETL2 used in subbasin
!!    spnd1       |none          |value for IPND1 used in subbasin
!!    spnd2       |none          |value for IPND2 used in subbasin
!!    spndesa     |ha            |value for PND_ESA used in subbasin
!!    spndev      |10^4 m^3 H2O  |value for PND_EVOL used in subbasin
!!    spndfr      |none          |value for PND_FR used in subbasin
!!    spndk       |mm/hr         |value for PND_K used in subbasin
!!    spndns      |mg/L          |value for PND_NSED used in subbasin
!!    spndpsa     |ha            |value for PND_PSA used in subbasin
!!    spndpv      |10^4 m^3 H2O  |value for PND_PVOL used in subbasin
!!    spnds       |mg/L          |value for PND_SED used in subbasin
!!    spndv       |10^4 m^3 H2O  |value for PND_VOL used in subbasin
!!    spno3       |mg N/L        |concentration of NO3 in pond
!!    sporgn      |mg N/L        |concentration of organic N in pond
!!    sporgp      |mg P/L        |concentration of organic P in pond
!!    spsolp      |mg P/L        |concentration of soluble P in pond
!!    sseci       |none          |value for SECCI used in subbasin
!!    sseciw      |none          |value for SECCIW used in subbasin
!!    sw1         |m/year        |value for PSETLW1 used in subbasin
!!    sw2         |m/year        |value for PSETLW2 used in subbasin
!!    swetfr      |none          |value for WET_FR used in subbasin
!!    swetk       |mm/hr         |value for WET_K used in subbasin
!!    swetmsa     |ha            |value for WET_MSA used in subbasin
!!    swetmv      |10^4 m^3 H2O  |value for WET_MVOL used in subbasin
!!    swetns      |mg/L          |value for WET_NSED used in subbasin
!!    swetnsa     |ha            |value for WET_NSA used in subbasin
!!    swetnv      |10^4 m^3 H2O  |value for WET_NVOL used in subbasin
!!    swets       |mg/L          |value fro WET_SED used in subbasin
!!    swetv       |10^4 m^3 H2O  |value for WET_VOL used in subbasin
!!    swno3       |mg N/L        |concentration of NO3 in water
!!    sworgn      |mg N/L        |concentration of organic N in water
!!    sworgp      |mg P/L        |concentration of organic P in water
!!    swsolp      |mg P/L        |concentration of soluble P in water
!!    titldum     |NA            |title line of .pnd file 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      character (len=80) :: titldum
      character (len=200) :: lus
      integer :: eof, j, sifld1, sifld2, sndt, spnd1, spnd2,  ib, k, ii    !!TSWAT
      real :: spndfr, spndpsa, spndpv, spndesa, spndev, spndv, spnds
      real :: spndns, spndk, swetfr, swetnsa, swetnv, swetmsa, sp1
      real :: swetmv, swetv, swets, swetns, swetk, sp2, sw1, sw2
      real :: sn1, sn2, snw1, snw2, schla, schlaw, sseci, sseciw
      real :: spno3, spsolp, sporgn, sporgp, swno3, swsolp, sworgn
      real :: sworgp, sub_ha, velsetlpnd, pndevcoeff, wetevcoeff, vselsetlpnd, pnd_d50, pnd_d50mm

      eof = 0
      spndfr = 0.
      spndpsa = 0.
      spndpv = 0.
      spndesa = 0.
      spndev = 0.
      spndv = 0.
      spnds = 0.
      spndns = 0.
      spndk = 0.
      sifld1 = 0
      sifld2 = 0
      sndt = 0
      sp1 = 0.
      sp2 = 0.
      sn1 = 0.
      sn2 = 0.
      schla = 0.
      sseci = 0.
      spno3 = 0.
      spsolp = 0.
      sporgn = 0.
      sporgp = 0.
      spnd1 = 0
      spnd2 = 0
      swetfr = 0.
      swetnsa = 0.
      swetnv = 0.
      swetmsa = 0.
      swetmv = 0.
      swetv = 0.
      swets = 0.
      swetns = 0.
      swetk = 0.
      sw1 = 0.
      sw2 = 0.
      snw1 = 0.
      snw2 = 0.
      schlaw = 0.
      sseciw = 0.
      swno3 = 0.
      swsolp = 0.
      sworgn = 0.
      sworgp = 0.
      pndevcoeff = 0. 
      wetevcoeff = 0.
      sub_ha = sub_km(i) * 100. 
      velsetlpnd = 0.

      do
      read (pndfile_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (pndfile_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spndfr        !| PND_FR : Fraction of subbasin area that drains into ponds. The value for PND_FR should be 
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spndpsa       !| PND_PSA: Surface area of ponds when filled to principal spillway [ha]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spndpv        !| PND_PVOL: Volume of water stored in ponds when filled to the principal spillway [104 m3]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spndesa       !| PND_ESA: Surface area of ponds when filled to emergency spillway [ha]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spndev        !| PND_EVOL: Volume of water stored in ponds when filled to the emergency spillway [104 m3]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spndv         !| PND_VOL: Initial volume of water in ponds [104 m3]
      if (eof < 0) exit 
      read (pndfile_num,*,iostat=eof) spnds         !| PND_SED: Initial sediment concentration in pond water [mg/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spndns        !| PND_NSED: Normal sediment concentration in pond water [mg/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spndk         !| PND_K: Hydraulic conductivity through bottom of ponds [mm/hr].
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sifld1        !| IFLOD1: Beginning month of non-flood season
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sifld2        !| IFLOD2: Ending month of non-flood season
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sndt          !| NDTARG: Number of days needed to reach target storage from current pond storage
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sp1           !| PSETLP1: Phosphorus settling rate in pond for months IPND1 through IPND2 [m/year]
      if (eof < 0) exit 
      read (pndfile_num,*,iostat=eof) sp2           !| PSETLP2: Phosphorus settling rate in pond for months other than IPND1-IPND2 [m/year]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sn1           !| NSETLP1: Initial dissolved oxygen concentration in the reach [mg O2/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sn2           !| NSETLP2: Initial dissolved oxygen concentration in the reach [mg O2/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) schla         !| CHLAP: Chlorophyll a production coefficient for ponds [ ] 
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sseci         !| SECCIP: Water clarity coefficient for ponds [m]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spno3         !| PND_NO3: Initial concentration of NO3-N in pond [mg N/l] 
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spsolp        !| PND_SOLP: Initial concentration of soluble P in pond [mg P/L]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sporgn        !| PND_ORGN: Initial concentration of organic N in pond [mg N/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sporgp        !| PND_ORGP: Initial concentration of organic P in pond [mg P/l]
      if (eof < 0) exit
      read (pndfile_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      if (titldum == '             '.or.titldum == 'Inputs used in')then 
        velsetlpnd = 10.0                           !!R679   3/6/20 nbs
      else
        backspace pndfile_num
        read (pndfile_num,*,iostat=eof) pnd_d50         !| PND_D50: Median particle diameter of sediment [um]
        pnd_d50mm = pnd_d50 / 1000.        !! micrometers to millimeters
        velsetlpnd = 24. * 411. * pnd_d50mm ** 2.
      endif    
      read (pndfile_num,*,iostat=eof) spnd1         !| IPND1: Beginning month of mid-year nutrient settling "season"
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) spnd2         !1    | IPND2: Ending month of mid-year nutrient settling "season"  
      if (eof < 0) exit
      read (pndfile_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swetfr        !| WET_FR : Fraction of subbasin area that drains into wetlands
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swetnsa       !| WET_NSA: Surface area of wetlands at normal water level [ha]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swetnv        !| WET_NVOL: Volume of water stored in wetlands when filled to normal water level [104 m3]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swetmsa       !| WET_MXSA: Surface area of wetlands at maximum water level [ha]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swetmv        !| WET_MXVOL: Volume of water stored in wetlands when filled to maximum water level [104 m3]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swetv         !| WET_VOL: Initial volume of water in wetlands [104 m3]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swets         !| WET_SED: Initial sediment concentration in wetland water [mg/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swetns        !| WET_NSED: Normal sediment concentration in wetland water [mg/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swetk         !| WET_K: Hydraulic conductivity of bottom of wetlands [mm/hr]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sw1           !| PSETLW1: Phosphorus settling rate in wetland for months IPND1 through IPND2 [m/year]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sw2           !| PSETLW2: Phosphorus settling rate in wetlands for months other than IPND1-IPND2 [m/year]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) snw1          !| NSETLW1: Nitrogen settling rate in wetlands for months IPND1 through IPND2 [m/year]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) snw2          !| NSETLW2: Nitrogen settling rate in wetlands for months other than IPND1-IPND2 [m/year]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) schlaw        !| CHLAW: Chlorophyll a production coefficient for wetlands [ ]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sseciw        !| SECCIW: Water clarity coefficient for wetlands [m]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swno3         !| WET_NO3: Initial concentration of NO3-N in wetland [mg N/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) swsolp        !| WET_SOLP: Initial concentration of soluble P in wetland [mg P/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sworgn        !| WET_ORGN: Initial concentration of organic N in wetland [mg N/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) sworgp        !| WET_ORGP: Initial concentration of organic P in wetland [mg P/l]
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) pndevcoeff    !| PNDEVCOEFF: Actual pond evaporation is equal to the potential evaporation times the pond evaporation coefficient
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) wetevcoeff    !| WETEVCOEFF: Actual wetland evaporation is equal to the potential evaporation times the wetland evaporation coefficient.
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) titldum
      if (eof < 0) exit
      read (pndfile_num,5101,iostat=eof) dpd_file
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) titldum
      if (eof < 0) exit
      read (pndfile_num,5101,iostat=eof) wpd_file
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) titldum
      if (eof < 0) exit
      read (pndfile_num,5101,iostat=eof) rib_file
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) titldum
      if (eof < 0) exit
      read (pndfile_num,5101,iostat=eof) sfb_file
      if (eof < 0) exit
      read (pndfile_num,*,iostat=eof) titldum
      if (eof < 0) exit
      read (pndfile_num,5101,iostat=eof) lid_file
      close (pndfile_num)
      exit
      end do

      !! Detention pond  -- read from a separate file (.dpd)
      if (dpd_file /= '             ' .and. ievent > 0) then
        open (dpd_file_num,file=dpd_file)
        do
        read (dpd_file_num,5100,iostat=eof) titldum
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_onoff(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_imo(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_iyr(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_evrsv(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_numweir(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_numstage(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_lwratio(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_totwrwid(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_stagdis(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_reltype(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_intcept(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_expont(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_coef1(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_coef2(i)
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) dtp_coef3(i)
        if (eof < 0) exit

        read (dpd_file_num,*,iostat=eof) (dtp_weirtype(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit 
        read (dpd_file_num,*,iostat=eof) (dtp_weirdim(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) (dtp_wdratio(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) (dtp_depweir(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) (dtp_diaweir(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) (dtp_addon(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) (dtp_flowrate(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) (dtp_cdis(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) (dtp_retperd(i,k),k=1,dtp_numstage(i))
        if (eof < 0) exit
        read (dpd_file_num,*,iostat=eof) (dtp_pcpret(i,k),k=1,dtp_numstage(i))   
        if (eof < 0) exit
        exit
        end do
        close (dpd_file_num)
      endif
     
!!  END DETENTION POND FILE

      !! Wet pond (.wpd file)
      if (wpd_file /= '             ' .and. ievent > 0) then
      open (wpd_file_num,file=wpd_file)
      do
      read (wpd_file_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_onoff(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_imo(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_iyr(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_k(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_evrsv(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_hydeff(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_dp(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_qi(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sedi(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sede(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_dim(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_pvol(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_pdepth(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sdslope(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_lenwdth(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_stagdis(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sdtype(i) 
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sdintc(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sdexp(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sdc1(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sdc2(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_sdc3(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_extdepth(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_pdia(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_plen(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_pmann(i)
      if (eof < 0) exit
      read (wpd_file_num,*,iostat=eof) wtp_ploss(i)
      if (eof < 0) exit
      exit
      end do
      close (wpd_file_num)
      endif   
      
      
!! end wet pond (.wpd file)

      !! Retention-Irrigation
      if (rib_file /= '             '.and. ievent > 0) then
      open (rib_file_num,file=rib_file)
      do
      read (rib_file_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) num_ri(i)
      if (eof < 0) exit
	read (rib_file_num,'(a200)',iostat=eof) lus
      if (eof < 0) exit

	do ii=2,len_trim(lus)
         num_noirr(i) = 1
         if (lus(ii:ii).eq.',' .or. lus(ii:ii).eq.' ') then
         if (lus(ii-1:ii-1).ne.' '.and.lus(ii-1:ii-1).ne.',') then
               num_noirr(i) = num_noirr(i) + 1
            end if
         end if	   
	end do 
	if (num_noirr(i)>0) then
	   backspace(rib_file_num)
	   read (rib_file_num,*) (ri_nirr(i,k), k=1,num_noirr(i))
         if (eof < 0) exit
      end if
     
      read (rib_file_num,5100,iostat=eof) titldum
      read (rib_file_num,*,iostat=eof) (ri_fr(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_dim(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_im(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_iy(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_sa(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_vol(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_qi(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_k(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_dd(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      read (rib_file_num,*,iostat=eof) (ri_evrsv(i,k),k=1,num_ri(i))
      if (eof < 0) exit
      close (rib_file_num)
      exit
      end do
      endif
!! end .rib file

      !! Sedimentaton-Filtration (.sfb file)
      if (sfb_file /= '             '.and. ievent > 0) then     
      open (sfb_file_num,file=sfb_file)
      do
      read (sfb_file_num,'(a20)',iostat=eof) titldum
      if (eof < 0) exit
      read (sfb_file_num,*,iostat=eof) num_sf(i)
      if (eof < 0) exit
      read (sfb_file_num,5100,iostat=eof) titldum
      
      read (sfb_file_num,*,iostat=eof) (sf_fr(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sf_typ(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sf_dim(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sf_ptp(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sf_im(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sf_iy(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_sa(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_pvol(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_qfg(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_pd(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_qi(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_bpw(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_k(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_dp(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_sedi(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (sp_sede(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_sa(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_fsa(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_qfg(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_pd(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_dep(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_bpw(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_k(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_dp(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_dc(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_h(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_por(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (tss_den(i,k),k=1,num_sf(i))
      read (sfb_file_num,*,iostat=eof) (ft_alp(i,k),k=1,num_sf(i))
      close (sfb_file_num)
      exit
      end do
      endif
          

!! end .sfb file

      !! LIDs (.lid file)
      !! LIDs (green roof, rain garden, cistern, and porous pavement) 
      if (lid_file /= '             '.and. ievent > 0) then     
      open (lid_file_num,file=lid_file)
      do
      read (lid_file_num,5100,iostat=eof) titldum
      read (lid_file_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      !! Green Roof (gr)
      read (lid_file_num,*,iostat=eof) (gr_onoff(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_imo(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_iyr(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_farea(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_solop(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_etcoef(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_fc(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_wp(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_ksat(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_por(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_hydeff(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_soldpt(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_dummy1(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_dummy2(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_dummy3(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_dummy4(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (gr_dummy5(i,k),k=1,mudb)
      if (eof < 0) exit
      !! Rain Garden (rg)
      read (lid_file_num,*,iostat=eof) (rg_onoff(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_imo(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_iyr(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_farea(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_solop(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_etcoef(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_fc(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_wp(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_ksat(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_por(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_hydeff(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_soldpt(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_dimop(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_sarea(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_vol(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_sth(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_sdia(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_bdia(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_sts(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_orifice(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_oheight(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_odia(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_dummy1(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_dummy2(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_dummy3(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_dummy4(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (rg_dummy5(i,k),k=1,mudb)
      !! CiStern (CS)
      read (lid_file_num,*,iostat=eof) (cs_onoff(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_imo(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_iyr(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_grcon(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_farea(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_vol(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_rdepth(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_dummy1(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_dummy2(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_dummy3(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_dummy4(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (cs_dummy5(i,k),k=1,mudb)
      !! Porous paVement (PV)
      read (lid_file_num,*,iostat=eof) (pv_onoff(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_imo(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_iyr(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_farea(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_grvdep(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_grvpor(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_solop(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_drcoef(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_fc(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_wp(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_ksat(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_por(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_hydeff(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_dummy1(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_dummy2(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_dummy3(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_dummy4(i,k),k=1,mudb)
      read (lid_file_num,*,iostat=eof) (pv_dummy5(i,k),k=1,mudb)
      close (lid_file_num)
      exit
      end do
      endif
!! end .lid file



      if (isproj == 2) swetfr = 0.0

!     set default values
      if (sndt <= 0) sndt = 15
      if (spndv > spndpv) spndv = spndpv
      if (swetv > swetnv) swetv = .95 * swetnv
      if (schla <= 0.) schla = 1.
      if (schlaw <= 0.) schlaw = 1.
      if (sseci <= 0.) sseci = 1.
      if (sseciw <= 0.) sseciw = 1.
      if (pndevcoeff <= 0.) pndevcoeff = 0.6
      if (wetevcoeff <= 0.) wetevcoeff = 0.6

!!    perform conversions
      sp1 = sp1 / 365.                     !! m/yr to m/day
      sp2 = sp2 / 365.
      sw1 = sw1 / 365.
      sw2 = sw2 / 365.
      sn1 = sn1 / 365.
      sn2 = sn2 / 365.
      snw1 = snw1 / 365.
      snw2 = snw2 / 365.
      spno3 = spno3 * spndv * 10.          !! mg/L to kg
      spsolp = spsolp * spndv * 10.
      sporgn = sporgn * spndv * 10.
      sporgp = sporgp * spndv * 10.
      swno3 = swno3 * spndv * 10.
      swsolp = swsolp * spndv * 10.
      sworgn = sworgn * spndv * 10.
      sworgp = sworgp * spndv * 10.

!! assign values to HRUs
      do j = 1, hrutot(i)
        ihru = 0
        ihru = nhru + j
        pnd_fr(ihru) = spndfr
        pnd_psa(ihru) = spndpsa
        pnd_pvol(ihru) = spndpv
        pnd_esa(ihru) = spndesa
        pnd_evol(ihru) = spndev
        pnd_vol(ihru) = spndv
        pnd_sed(ihru) = spnds
        pnd_nsed(ihru) = spndns
        pnd_k(ihru) = spndk
        iflod1(ihru) = sifld1
        iflod2(ihru) = sifld2
        ndtarg(ihru) = sndt
        psetlp(1,ihru) = sp1
        psetlp(2,ihru) = sp2
        nsetlp(1,ihru) = sn1
        nsetlp(2,ihru) = sn2
        chlap(ihru) = schla
        seccip(ihru) = sseci
        pnd_no3(ihru) = spno3
        pnd_solp(ihru) = spsolp
        pnd_orgn(ihru) = sporgn
        pnd_orgp(ihru) = sporgp
        ipnd1(ihru) = spnd1
        ipnd2(ihru) = spnd2
        velsetlp(ihru) = velsetlpnd
        wet_fr(ihru) = swetfr
        wet_nsa(ihru) = swetnsa
        wet_nvol(ihru) = swetnv
        wet_mxsa(ihru) = swetmsa
        wet_mxvol(ihru) = swetmv
        wet_vol(ihru) = swetv
        wet_sed(ihru) = swets
        wet_nsed(ihru) = swetns
        wet_k(ihru) = swetk
        psetlw(1,ihru) = sw1
        psetlw(2,ihru) = sw2
        nsetlw(1,ihru) = snw1
        nsetlw(2,ihru) = snw2
        chlaw(ihru) = schlaw
        secciw(ihru) = sseciw
        wet_no3(ihru) = swno3
        wet_solp(ihru) = swsolp
        wet_orgn(ihru) = sworgn
        wet_orgp(ihru) = sworgp
        evpnd(ihru) = pndevcoeff
        evwet(ihru) = wetevcoeff
      
      
          if (nswat ==1) then   !!calibrating qichun 's nitrification and denitrification method 
             if (swetnsa <=0.) swetnsa = 0.15
            if (swetnv <=0.) swetnv = 0.002
            if (swetmsa <=0.) swetmsa = 0.015
            if (swetmv <=0.) swetmv = 0.1
            if (swets <=0.) swets = 1.1
           
            MaxRate(ihru)= swetnsa    !! 0.15
            N2Oadjust_wp(ihru)= swetnv    !0.002
            N2Oadjust_fc(ihru)= swetmsa   !0.015
            min_nitrate(ihru)= swetmv   !0.1
            wfpsdnitadj(ihru) = swets   ! 1.1    |value fro WET_SED used in subbasin
           
           end if 
      end do

!!    close (104)
      
      !! Set default values for urban BMP parameters
      if (ievent > 0) then
        call bmpinit
        call lidinit
      endif
            
      return
 5100 format (a)
 5101 format(a13)   
      end