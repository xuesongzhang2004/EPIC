      subroutine hhwatqual

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient transformations and water
!!    quality calculations for hourly timestep

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0              |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    ai1              |mg N/mg alg   |fraction of algal biomass that is N
!!    ai2              |mg P/mg alg   |fraction of algal biomass that is P
!!    ai3              |mg O2/mg alg  |the rate of oxygen production per unit of
!!                                    |algal photosynthesis
!!    ai4              |mg O2/mg alg  |the rate of oxygen uptake per unit of
!!                                    |algae respiration
!!    ai5              |mg O2/mg N    |the rate of oxygen uptake per unit of NH3
!!                                    |nitrogen oxidation
!!    ai6              |mg O2/mg N    |the rate of oxygen uptake per unit of NO2
!!                                    |nitrogen oxidation
!!    rch_Alg(:)         |mg alg/L      |algal biomass concentration in reach
!!    rch_NH4(:)      |mg N/L        |ammonia concentration in reach
!!    bc1(:)           |1/hr          |rate constant for biological oxidation of
!!                                    |NH3 to NO2 in reach at 20 deg C
!!    bc2(:)           |1/hr          |rate constant for biological oxidation of
!!                                    |NO2 to NO3 in reach at 20 deg C
!!    bc3(:)           |1/hr          |rate constant for hydrolysis of organic N
!!                                    |to ammonia in reach at 20 deg C
!!    bc4(:)           |1/hr          |rate constant for the decay of organic P
!!                                    |to dissolved P in reach at 20 deg C
!!    rch_Chla(:)        |mg chl-a/L    |chlorophyll-a concentration in reach
!!    dayl(:)          |hours         |day length for current day
!!    rch_SolP(:)       |mg P/L        |dissolved P concentration in reach
!!    frad(:,:)        |none          |fraction of solar radiation occuring 
!!                                    |during hour in day in HRU
!!    hdepth(:)        |m             |depth of flow on day
!!    hhtime(:)        |hr            |flow travel time for hour
!!    hhvaroute(2,:,:) |m^3 H2O       |water
!!    hhvaroute(4,:,:) |kg N          |organic nitrogen
!!    hhvaroute(5,:,:) |kg P          |organic posphorus
!!    hhvaroute(6,:,:) |kg N          |nitrate
!!    hhvaroute(7,:,:) |kg P          |soluble phosphorus
!!    hhvaroute(13,:,:)|kg            |chlorophyll-a
!!    hhvaroute(14,:,:)|kg N          |ammonium
!!    hhvaroute(15,:,:)|kg N          |nitrite
!!    hhvaroute(16,:,:)|kg            |carbonaceous biological oxygen demand
!!    hhvaroute(17,:,:)|kg O2         |dissolved oxygen
!!    hrchwtr(ii)      |m^3 H2O       |water stored in reach at beginning of day
!!    hrtwtr(:)        |m^3 H2O       |flow out of reach
!!    hru_ra(:)        |MJ/m^2        |solar radiation for the day in HRU
!!    igropt           |none          |Qual2E option for calculating the local
!!                                    |specific growth rate of algae
!!                                    |1: multiplicative:
!!                                    | u = mumax * fll * fnn * fpp
!!                                    |2: limiting nutrient
!!                                    | u = mumax * fll * Min(fnn, fpp)
!!                                    |3: harmonic mean
!!                                    | u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
!!    inum1            |none          |reach number
!!    inum2            |none          |inflow hydrograph storage location number
!!    k_l              |MJ/(m2*hr)    |half saturation coefficient for light
!!    k_n              |mg N/L        |michaelis-menton half-saturation constant
!!                                    |for nitrogen
!!    k_p              |mg P/L        |michaelis-menton half saturation constant
!!                                    |for phosphorus
!!    lambda0          |1/m           |non-algal portion of the light extinction
!!                                    |coefficient
!!    lambda1          |1/(m*ug chla/L)|linear algal self-shading coefficient
!!    lambda2          |(1/m)(ug chla/L)**(-2/3)
!!                                    |nonlinear algal self-shading coefficient
!!    mumax            |1/hr          |maximum specific algal growth rate at 
!!                                    |20 deg C
!!    rch_NO3(:)      |mg N/L        |nitrate concentration in reach
!!    rch_NO2(:)      |mg N/L        |nitrite concentration in reach
!!    rch_OrgN(:)      |mg N/L        |organic nitrogen concentration in reach
!!    rch_OrgP(:)      |mg P/L        |organic phosphorus concentration in reach
!!    p_n              |none          |algal preference factor for ammonia
!!    rch_CBOD(:)      |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                    |reach 
!!    rch_DOX(:)       |mg O2/L       |dissolved oxygen concentration in reach
!!    rhoq             |1/hr          |algal respiration rate at 20 deg C
!!    rk1(:)           |1/hr          |CBOD deoxygenation rate coefficient in 
!!                                    |reach at 20 deg C
!!    rk2(:)           |1/hr          |reaeration rate in accordance with Fickian
!!                                    |diffusion in reach at 20 deg C
!!    rk3(:)           |1/hr          |rate of loss of CBOD due to settling in 
!!                                    |reach at 20 deg C
!!    rk4(:)           |mg O2/        |sediment oxygen demand rate in reach
!!                     |  ((m**2)*hr) |at 20 deg C
!!    rnum1            |none          |fraction of overland flow
!!    rs1(:)           |m/hr          |local algal settling rate in reach at 
!!                                    |20 deg C
!!    rs2(:)           |(mg disP-P)/  |benthos source rate for dissolved P
!!                     |  ((m**2)*hr) |in reach at 20 deg C
!!    rs3(:)           |(mg NH4-N)/   |benthos source rate for ammonia nitrogen
!!                     |  ((m**2)*hr) |in reach at 20 deg C
!!    rs4(:)           |1/hr          |rate coefficient for organic nitrogen
!!                                    |settling in reach at 20 deg C
!!    rs5(:)           |1/hr          |organic phosphorus settling rate in reach
!!                                    |at 20 deg C
!!    rttime           |hr            |reach travel time
!!    tfact            |none          |fraction of solar radiation that is
!!                                    |photosynthetically active
!!    tmpav(:)         |deg C         |average air temperature on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rch_Alg(:)    |mg alg/L      |algal biomass concentration in reach
!!    rch_NH4(:) |mg N/L        |ammonia concentration in reach
!!    rch_Chla(:)   |mg chl-a/L    |chlorophyll-a concentration in reach
!!    rch_SolP(:)  |mg P/L        |dissolved phosphorus concentration in reach
!!    rch_NO3(:) |mg N/L        |nitrate concentration in reach
!!    rch_NO2(:) |mg N/L        |nitrite concentration in reach
!!    rch_OrgN(:) |mg N/L        |organic nitrogen concentration in reach
!!    rch_OrgP(:) |mg P/L        |organic phosphorus concentration in reach
!!    rch_CBOD(:) |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                               |reach
!!    rch_DOX(:)  |mg O2/L       |dissolved oxygen concentration in reach
!!    soxy        |mg O2/L       |saturation concetration of dissolved oxygen
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algcon      |mg alg/L      |initial algal biomass concentration in reach
!!    algi        |MJ/(m2*hr)    |photosynthetically active light intensity
!!                               |for hour
!!    algin       |mg alg/L      |algal biomass concentration in inflow
!!    ammoin      |mg N/L        |ammonium N concentration in inflow
!!    bc1mod      |1/day         |rate constant for biological oxidation of NH3
!!                               |to NO2 modified to reflect impact of low 
!!                               |oxygen concentration
!!    bc2mod      |1/day         |rate constant for biological oxidation of NO2
!!                               |to NO3 modified to reflect impact of low
!!                               |oxygen concentration
!!    cbodcon     |mg/L          |initial carbonaceous biological oxygen demand
!!                               |concentration in reach
!!    cbodin      |mg/L          |carbonaceous biological oxygen demand 
!!                               |concentration in inflow
!!    chlin       |mg chl-a/L    |chlorophyll-a concentration in inflow
!!    cinn        |mg N/L        |effective available nitrogen concentration
!!    cordo       |none          |nitrification rate correction factor
!!    disoxin     |mg O2/L       |dissolved oxygen concentration in inflow
!!    dispin      |mg P/L        |soluble P concentration in inflow
!!    f1          |none          |fraction of algal nitrogen uptake from
!!                               |ammonia pool
!!    fll         |none          |growth attenuation factor for light
!!    fnn         |none          |algal growth limitation factor for nitrogen
!!    fpp         |none          |algal growth limitation factor for phosphorus
!!    gra         |1/hr          |local algal growth rate at 20 deg C
!!    jrch        |none          |reach number
!!    lambda      |1/m           |light extinction coefficient
!!    nh3con      |mg N/L        |initial ammonia concentration in reach
!!    nitratin    |mg N/L        |nitrate concentration in inflow
!!    nitritin    |mg N/L        |nitrite concentration in inflow
!!    no2con      |mg N/L        |initial nitrite concentration in reach
!!    no3con      |mg N/L        |initial nitrate concentration in reach
!!    o2con       |mg O2/L       |initial dissolved oxygen concentration in 
!!                               |reach
!!    orgncon     |mg N/L        |initial organic N concentration in reach
!!    orgnin      |mg N/L        |organic N concentration in inflow
!!    orgpcon     |mg P/L        |initial organic P concentration in reach
!!    orgpin      |mg P/L        |organic P concentration in inflow
!!    solpcon     |mg P/L        |initial soluble P concentration in reach
!!    thbc1       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NH3 to NO2
!!    thbc2       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NO2 to NO3
!!    thbc3       |none          |temperature adjustment factor for local
!!                               |hydrolysis of organic N to ammonia N
!!    thbc4       |none          |temperature adjustment factor for local
!!                               |decay of organic P to dissolved P
!!    thgra       |none          |temperature adjustment factor for local algal
!!                               |growth rate
!!    thour       |none          |flow duration (fraction of hr)
!!    thrho       |none          |temperature adjustment factor for local algal
!!                               |respiration rate
!!    thrk1       |none          |temperature adjustment factor for local CBOD
!!                               |deoxygenation
!!    thrk2       |none          |temperature adjustment factor for local oxygen
!!                               |reaeration rate
!!    thrk3       |none          |temperature adjustment factor for loss of
!!                               |CBOD due to settling
!!    thrk4       |none          |temperature adjustment factor for local
!!                               |sediment oxygen demand
!!    thrs1       |none          |temperature adjustment factor for local algal
!!                               |settling rate
!!    thrs2       |none          |temperature adjustment factor for local
!!                               |benthos source rate for dissolved phosphorus
!!    thrs3       |none          |temperature adjustment factor for local
!!                               |benthos source rate for ammonia nitrogen
!!    thrs4       |none          |temperature adjustment factor for local
!!                               |organic N settling rate
!!    thrs5       |none          |temperature adjustment factor for local
!!                               |organic P settling rate
!!    wtmp        |deg C         |temperature of water in reach
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    uu          |varies        |variable to hold intermediate calculation
!!                               |result
!!    vv          |varies        |variable to hold intermediate calculation
!!                               |result
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ww          |varies        |variable to hold intermediate calculation
!!                               |result
!!    xx          |varies        |variable to hold intermediate calculation
!!                               |result
!!    yy          |varies        |variable to hold intermediate calculation
!!                               |result
!!    zz          |varies        |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp, Min
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use parm_rchC
      use parm_control
      implicit none
      
      integer :: jrch, ii
      real :: wtrin, chlin, algin, orgnin, ammoin, nitratin, nitritin
      real :: orgpin, dispin, cbodin, disoxin, thour, wtmp, fll, gra
      real :: lambda, fnn, fpp, algi, xx, yy, zz, ww, cinn
      real :: uu, vv, cordo, f1, algcon, orgncon, nh3con, no2con, no3con
      real :: orgpcon, solpcon, cbodcon, o2con, wtrtot, bc1mod, bc2mod
      real :: thgra = 1.047, thrho = 1.047, thrs1 = 1.024
      real :: thrs2 = 1.074, thrs3 = 1.074, thrs4 = 1.024, thrs5 = 1.024
      real :: thbc1 = 1.083, thbc2 = 1.047, thbc3 = 1.047, thbc4 = 1.047
      real :: thrk1 = 1.047, thrk2 = 1.024, thrk3 = 1.024, thrk4 = 1.060
!      real :: thrk5 = 1.047, thrk6 = 1.0, thrs6 = 1.024, thrs7 = 1.0
      real, external :: theta

      jrch = 0
      jrch = inum1

!! hourly loop
      do ii = 1, nstep_rch
       !! initialize water flowing into reach
       wtrin = 0.
       wtrin = hhvaroute(2,inum2,ii) * (1. - rnum1)

       if (hrtwtr(ii) / (idt_rch * 60.) > 0.01 .and. hdepth(ii) > 0.01) then  !!R672 6/19/19 nbs
!! concentrations
         !! initialize inflow concentrations
         chlin = 0.
         algin = 0.
         orgnin = 0.
         ammoin = 0.
         nitritin = 0.
         nitratin = 0.
         orgpin = 0.
         dispin = 0.
         cbodin = 0.
         disoxin = 0.
         cinn = 0.                                  !!    cinn        |mg N/L        |effective available nitrogen concentration
         if (wtrin > 0.001) then
             chlin = 1000. * hhvaroute(13,inum2,ii) * (1. - rnum1) / wtrin
             algin = 1000. * chlin / ai0        !! QUAL2E equation III-1    !!    ai0              |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
             orgnin = 1000. * hhvaroute(4,inum2,ii) * (1. - rnum1) / wtrin
             ammoin = 1000. * hhvaroute(14,inum2,ii) * (1. - rnum1) / wtrin
             nitritin = 1000. * hhvaroute(15,inum2,ii) * (1. - rnum1) / wtrin
             nitratin = 1000. * hhvaroute(6,inum2,ii) * (1. - rnum1) / wtrin
             orgpin = 1000. * hhvaroute(5,inum2,ii) * (1. - rnum1) / wtrin
             dispin = 1000. * hhvaroute(7,inum2,ii) * (1. - rnum1) / wtrin
             cbodin = 1000. * hhvaroute(16,inum2,ii) * (1. - rnum1) / wtrin
             disoxin= 1000. * hhvaroute(17,inum2,ii) * (1. - rnum1) / wtrin
         end if

         if (chlin < 1.e-6) chlin = 0.0
         if (algin < 1.e-6) algin = 0.0
         if (orgnin < 1.e-6) orgnin = 0.0
         if (ammoin < 1.e-6) ammoin = 0.0
         if (nitritin < 1.e-6) nitritin = 0.0
         if (nitratin < 1.e-6) nitratin = 0.0
         if (orgpin < 1.e-6) orgpin = 0.0
         if (dispin < 1.e-6) dispin = 0.0
         if (cbodin < 1.e-6) cbodin = 0.0
         if (disoxin < 1.e-6) disoxin = 0.0

         !! initialize concentration of nutrient in reach
         wtrtot = 0.
         algcon = 0.
         orgncon = 0.
         nh3con = 0.
         no2con = 0.
         no3con = 0.
         orgpcon = 0.
         solpcon = 0.
         cbodcon = 0.
         o2con = 0.
         wtrtot = wtrin + hrchwtr(ii)
         if (wtrtot > 0.01) then                     !!R671 4/5/19  nbs
           if (ii == 1) then
             algcon = (algin * wtrin + rch_Alg(jrch) * hrchwtr(ii)) / wtrtot
             orgncon = (orgnin * wtrin + rch_OrgN(jrch) * hrchwtr(ii)) / wtrtot
             nh3con = (ammoin * wtrin + rch_NH4(jrch) * hrchwtr(ii)) / wtrtot
             no2con = (nitritin * wtrin + rch_NO2(jrch) * hrchwtr(ii)) / wtrtot
             no3con = (nitratin * wtrin + rch_NO3(jrch) * hrchwtr(ii)) / wtrtot
             orgpcon = (orgpin * wtrin + rch_OrgP(jrch) * hrchwtr(ii)) / wtrtot
             solpcon = (dispin * wtrin + rch_SolP(jrch) * hrchwtr(ii)) / wtrtot
             cbodcon = (cbodin * wtrin + rch_CBOD(jrch) * hrchwtr(ii)) / wtrtot
             o2con = (disoxin * wtrin + rch_DOX(jrch) * hrchwtr(ii)) / wtrtot
         else
             algcon = (algin * wtrin + rch_hAlg(ii-1) * hrchwtr(ii)) / wtrtot
             orgncon = (orgnin * wtrin + rch_hOrgN(ii-1) * hrchwtr(ii)) / wtrtot
             nh3con = (ammoin * wtrin + rch_hNH4(ii-1) * hrchwtr(ii)) / wtrtot
             no2con = (nitritin * wtrin + rch_hNO2(ii-1) * hrchwtr(ii)) / wtrtot
             no3con = (nitratin * wtrin + rch_hNO3(ii-1) * hrchwtr(ii)) / wtrtot
             orgpcon = (orgpin * wtrin + rch_hOrgP(ii-1) * hrchwtr(ii)) / wtrtot
             solpcon = (dispin * wtrin + rch_hSolP(ii-1) * hrchwtr(ii)) / wtrtot
             cbodcon = (cbodin * wtrin + rch_hCBOD(ii-1) * hrchwtr(ii)) / wtrtot
             o2con = (disoxin * wtrin + rch_hDOX(ii-1) * hrchwtr(ii)) / wtrtot
           end if 
         end if                                         !!R671 4/5/19  nbs

         if (algcon < 1.e-6) algcon = 0.0
         if (orgncon < 1.e-6) orgncon = 0.0
         if (nh3con < 1.e-6) nh3con = 0.0
         if (no2con < 1.e-6) no2con = 0.0
         if (no3con < 1.e-6) no3con = 0.0
         if (orgpcon < 1.e-6) orgpcon = 0.0
         if (solpcon < 1.e-6) solpcon = 0.0
         if (cbodcon < 1.e-6) cbodcon = 0.0
         if (o2con < 1.e-6) o2con = 0.0
         !! calculate temperature in stream
         !! Stefan and Preudhomme. 1993.  Stream temperature estimation 
         !! from air temperature.  Water Res. Bull. p. 27-45
         !! SWAT manual equation 2.3.13
         wtmp = 0.
         wtmp = 5.0 + 0.75 * tmpav(jrch)                        !!    tmpav(:)         |deg C         |average air temperature on current day
         if (wtmp <= 0.) wtmp = 0.1

         !! calculate effective concentration of available nitrogen
         !! QUAL2E equation III-15
         cinn = nh3con + no3con                                 !!    cinn        |mg N/L        |effective available nitrogen concentration

         !! calculate saturation concentration for dissolved oxygen
         !! QUAL2E section 3.6.1 equation III-29
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         ww = -139.34410 + (1.575701e05 / (wtmp + 273.15))
         xx = 6.642308e07 / ((wtmp + 273.15)**2)
         yy = 1.243800e10 / ((wtmp + 273.15)**3)
         zz = 8.621949e11 / ((wtmp + 273.15)**4)
         soxy = Exp(ww - xx + yy - zz)
         if (soxy < 0.) soxy = 0.
!! end initialize concentrations

!! O2 impact calculations
        !! calculate nitrification rate correction factor for low
        !! oxygen QUAL2E equation III-21
        cordo = 0.                          !!    cordo       |none          |nitrification rate correction factor
        cordo = 1.0 - Exp(-0.6 * o2con)
        !! modify ammonia and nitrite oxidation rates to account for
        !! low oxygen
        bc1mod = 0.                     !!    bc1mod      |1/day         |rate constant for biological oxidation of NH3
!!                                                                          |to NO2 modified to reflect impact of low 
!!                                                                          |oxygen concentration                                                     
        bc2mod = 0.                     !!    bc2mod      |1/day         |rate constant for biological oxidation of NO2
!!                                                                          |to NO3 modified to reflect impact of low
!!                                                                          |oxygen concentration
        bc1mod = bc1(jrch) * cordo      !!    bc1(:)           |1/hr          |rate constant for biological oxidation of NH3 to NO2 in reach at 20 deg C
        !!    cordo       |none          |nitrification rate correction factor
        bc2mod = bc2(jrch) * cordo      !!    bc2(:)           |1/hr          |rate constant for biological oxidation of NO2 to NO3 in reach at 20 deg C
        !!    cordo       |none          |nitrification rate correction factor
!! end O2 impact calculations

         !! calculate flow duration
         thour = 0.                     !!    thour       |none          |flow duration (fraction of hr)
         thour = hhtime(ii)
         if (thour > 1.0) thour = 1.0
         thour = 1.0

!! algal growth
         !! calculate light extinction coefficient 
         !! (algal self shading) QUAL2E equation III-12
         if (ai0 * algcon > 1.e-6) then                 !!    ai0              |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
            lambda = lambda0 + (lambda1 * ai0 * algcon) + lambda2 * (ai0 * algcon) ** (.66667)
            !!    lambda      |1/m           |light extinction coefficient
            !!    lambda0          |1/m           |non-algal portion of the light extinction coefficient
            !!    lambda1          |1/(m*ug chla/L)|linear algal self-shading coefficient
            !!    lambda2          |(1/m)(ug chla/L)**(-2/3)
         else
           lambda = lambda0
         endif

         !! calculate algal growth limitation factors for nitrogen
         !! and phosphorus QUAL2E equations III-13 & III-14
         fnn = 0.                               !!    fnn         |none          |algal growth limitation factor for nitrogen
         fpp = 0.                               !!    fpp         |none          |algal growth limitation factor for phosphorus
         fnn = cinn / (cinn + k_n)              !!    k_n              |mg N/L        |michaelis-menton half-saturation constant for nitrogen
         !!    cinn        |mg N/L        |effective available nitrogen concentration
         fpp = solpcon / (solpcon + k_p)        !!    k_p              |mg P/L        |michaelis-menton half saturation constant for phosphorus
        
         !! calculate hourly, photosynthetically active,
         !! light intensity QUAL2E equation III-9c
         !! Light Averaging Option # 3
         algi = 0.                                                      !!    algi        |MJ/(m2*hr)    |photosynthetically active light intensity for hour
         algi = frad(hru1(jrch),ii) * hru_ra(hru1(jrch)) * tfact        !!    tfact            |none          |fraction of solar radiation that is photosynthetically active

         !! calculate growth attenuation factor for light, based on
         !! hourly light intensity QUAL2E equation III-6a
         fll = 0.               !!    fll         |none          |growth attenuation factor for light
         fll = (1. / (lambda * hdepth(ii))) * Log((k_l + algi) / (k_l + algi * (Exp(-lambda * hdepth(ii)))))
         !!    k_l              |MJ/(m2*hr)    |half saturation coefficient for light
         !!    algi        |MJ/(m2*hr)    |photosynthetically active light intensity for hour

         !! calculcate local algal growth rate
         gra = 0.                   !!    gra         |1/hr          |local algal growth rate at 20 deg C
         select case (igropt)       !!!!    igropt           |none          |Qual2E option for calculating the local
!!                                    |specific growth rate of algae
!!                                    |1: multiplicative:
!!                                    | u = mumax * fll * fnn * fpp
!!                                    |2: limiting nutrient
!!                                    | u = mumax * fll * Min(fnn, fpp)
!!                                    |3: harmonic mean
!!                                    | u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
           case (1)
             !! multiplicative QUAL2E equation III-3a
             gra = mumax * fll * fnn * fpp              !!    fll         |none          |growth attenuation factor for light
             !!    fnn         |none          |algal growth limitation factor for nitrogen
             !!    fpp         |none          |algal growth limitation factor for phosphorus
             !!    gra         |1/hr          |local algal growth rate at 20 deg C
           case (2)
             !! limiting nutrient QUAL2E equation III-3b
             gra = mumax * fll * Min(fnn, fpp)          !!    fll         |none          |growth attenuation factor for light
             !!    fnn         |none          |algal growth limitation factor for nitrogen
             !!    fpp         |none          |algal growth limitation factor for phosphorus
             !!    gra         |1/hr          |local algal growth rate at 20 deg C
           case (3)
             !! harmonic mean QUAL2E equation III-3c
             if (fnn > 1.e-6 .and. fpp > 1.e-6) then
               gra = mumax * fll * 2. / ((1. / fnn) + (1. / fpp))       !!    fll         |none          |growth attenuation factor for light
               !!    fnn         |none          |algal growth limitation factor for nitrogen
               !!    fpp         |none          |algal growth limitation factor for phosphorus
               !!    gra         |1/hr          |local algal growth rate at 20 deg C
             else
               gra = 0.
             endif
         end select

         !! calculate algal biomass concentration at end of day
         !! (phytoplanktonic algae)
         !! QUAL2E equation III-2
         rch_hAlg(ii) = 0.
         rch_hAlg(ii) = algcon + (Theta(gra,thgra,wtmp) * algcon -        &
                                  Theta(rhoq,thrho,wtmp) * algcon -         &
                                  Theta(rs1(jrch),thrs1,wtmp)  / hdepth(ii) * algcon) * thour           !!    thour       |none          |flow duration (fraction of hr)
         !!    rhoq             |1/hr          |algal respiration rate at 20 deg C
         !!    rs1(:)           |m/hr          |local algal settling rate in reach at 20 deg C
         !!    gra         |1/hr          |local algal growth rate at 20 deg C
         !!    thgra       |none          |temperature adjustment factor for local algal growth rate
         !!    thrho       |none          |temperature adjustment factor for local algal respiration rate
         !!    thrs1       |none          |temperature adjustment factor for local algal settling rate
         if (rch_hAlg(ii) < 0.) rch_hAlg(ii) = 0.

         !! calculate chlorophyll-a concentration at end of day
         !! QUAL2E equation III-1
         rch_hChla(ii) = 0.
         rch_hChla(ii) = rch_hAlg(ii) * ai0 / 1000.     !!    ai0              |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!! end algal growth 

!! oxygen calculations
         !! calculate carbonaceous biological oxygen demand at end
         !! of day QUAL2E section 3.5 equation III-26
         yy = 0.
         zz = 0.
         yy = Theta(rk1(jrch),thrk1,wtmp) * cbodcon         !!    rk1(:)           |1/hr          |CBOD deoxygenation rate coefficient in reach at 20 deg C
         !!    thrk1       |none          |temperature adjustment factor for local CBOD deoxygenation
         zz = Theta(rk3(jrch),thrk3,wtmp) * cbodcon         !!    rk3(:)           |1/hr          |rate of loss of CBOD due to settling in reach at 20 deg C
         !!    thrk3       |none          |temperature adjustment factor for loss of CBOD due to settling
         rch_hCBOD(ii) = 0.
         rch_hCBOD(ii) = cbodcon - (yy + zz) * thour        !!    thour       |none          |flow duration (fraction of hr)
         if (rch_hCBOD(ii) < 0.) rch_hCBOD(ii) = 0.

         !! calculate dissolved oxygen concentration if reach at 
         !! end of day QUAL2E section 3.6 equation III-28
         uu = 0.
         vv = 0.
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         uu = Theta(rk2(jrch),thrk2,wtmp) * (soxy - o2con)          !!    rk2(:)           |1/hr          |reaeration rate in accordance with Fickian diffusion in reach at 20 deg C
         !!    thrk2       |none          |temperature adjustment factor for local oxygen reaeration rate
         vv = (ai3 * Theta(gra,thgra,wtmp) - ai4 * Theta(rhoq,thrho,wtmp)) * algcon
         !!    ai3              |mg O2/mg alg  |the rate of oxygen production per unit ofalgal photosynthesis
         !!    ai4              |mg O2/mg alg  |the rate of oxygen uptake per unit of algae respiration
         !!    rhoq             |1/hr          |algal respiration rate at 20 deg C
         !!    gra         |1/hr          |local algal growth rate at 20 deg C
         !!    thgra       |none          |temperature adjustment factor for local algal growth rate
         !!    thrho       |none          |temperature adjustment factor for local algal respiration rate
         ww = Theta(rk1(jrch),thrk1,wtmp) * cbodcon                         !!    rk1(:)           |1/hr          |CBOD deoxygenation rate coefficient in reach at 20 deg C
         !!    thrk1       |none          |temperature adjustment factor for local CBOD deoxygenation
         xx = Theta(rk4(jrch),thrk4,wtmp) / (hdepth(ii) * 1000.)            !!    rk4(:)           |mg O2/        |sediment oxygen demand rate in reach ((m**2)*hr) |at 20 deg C
         !!    thrk4       |none          |temperature adjustment factor for local sediment oxygen demand
         yy = ai5 * Theta(bc1mod,thbc1,wtmp) * nh3con       !!    ai5              |mg O2/mg N    |the rate of oxygen uptake per unit of NH3 nitrogen oxidation
         !!    bc1mod      |1/day         |rate constant for biological oxidation of NH3 to NO2 modified to reflect impact of low  oxygen concentration
         !!    thbc1       |none          |temperature adjustment factor for local biological oxidation of NH3 to NO2
         zz = ai6 * Theta(bc2mod,thbc2,wtmp) * no2con       !!    ai6              |mg O2/mg N    |the rate of oxygen uptake per unit of NO2 nitrogen oxidation
         !!    bc2mod      |1/day         |rate constant for biological oxidation of NO2 to NO3 modified to reflect impact of low oxygen concentration
         !!    thbc2       |none          |temperature adjustment factor for local biological oxidation of NO2 to NO3
         rch_hDOX(ii) = 0.
         rch_hDOX(ii) = o2con + (uu + vv - ww - xx - yy - zz) * thour
         if (rch_hDOX(ii) < 0.) rch_hDOX(ii) = 0.
!! end oxygen calculations

!! nitrogen calculations
         !! calculate organic N concentration at end of day
         !! QUAL2E section 3.3.1 equation III-16
         xx = 0.
         yy = 0.
         zz = 0.
         xx = ai1 * Theta(rhoq,thrho,wtmp) * algcon         !!    ai1              |mg N/mg alg   |fraction of algal biomass that is N
        !!    rhoq             |1/hr          |algal respiration rate at 20 deg C
        !!    thrho       |none          |temperature adjustment factor for local algal respiration rate
         yy = Theta(bc3(jrch),thbc3,wtmp) * orgncon         !!    bc3(:)           |1/hr          |rate constant for hydrolysis of organic N to ammonia in reach at 20 deg C
         !!    thbc3       |none          |temperature adjustment factor for local hydrolysis of organic N to ammonia N
         zz = Theta(rs4(jrch),thrs4,wtmp) * orgncon         !!    rs4(:)           |1/hr          |rate coefficient for organic nitrogen settling in reach at 20 deg C
         !!    thrs4       |none          |temperature adjustment factor for local organic N settling rate
         rch_hOrgN(ii) = 0.
         rch_hOrgN(ii) = orgncon + (xx - yy - zz) * thour   !!    thour       |none          |flow duration (fraction of hr)
         if (rch_hOrgN(ii) < 0.) rch_hOrgN(ii) = 0.

        !! calculate fraction of algal nitrogen uptake from ammonia
        !! pool QUAL2E equation III-18
        f1 = 0.                     !!    f1          |none          |fraction of algal nitrogen uptake from ammonia pool
        f1 = p_n * nh3con / (p_n * nh3con + (1. - p_n) * no3con + 1.e-6)

        !! calculate ammonia nitrogen concentration at end of day
        !! QUAL2E section 3.3.2 equation III-17
        ww = 0.
        xx = 0.
        yy = 0.
        zz = 0.
        ww = Theta(bc3(jrch),thbc3,wtmp) * orgncon                  !!    bc3(:)           |1/hr          |rate constant for hydrolysis of organic N to ammonia in reach at 20 deg C
        !!    thbc3       |none          |temperature adjustment factor for local hydrolysis of organic N to ammonia N
        xx = Theta(bc1mod,thbc1,wtmp) * nh3con                      !!    bc1(:)           |1/hr          |rate constant for biological oxidation of NH3 to NO2 in reach at 20 deg C
        !!    bc1mod      |1/day         |rate constant for biological oxidation of NH3 to NO2 modified to reflect impact of low oxygen concentration
        !!    thbc1       |none          |temperature adjustment factor for local biological oxidation of NH3 to NO2
        yy = Theta(rs3(jrch),thrs3,wtmp) / (hdepth(ii) * 1000.)     !!    rs3(:)           |(mg NH4-N)/   |benthos source rate for ammonia nitrogen ((m**2)*hr) |in reach at 20 deg C
        !!    thrs3       |none          |temperature adjustment factor for local benthos source rate for ammonia nitrogen
        zz = f1 * ai1 * algcon * Theta(gra,thgra,wtmp)              !!    ai1              |mg N/mg alg   |fraction of algal biomass that is N
        !!    f1          |none          |fraction of algal nitrogen uptake from ammonia pool
        !!    gra         |1/hr          |local algal growth rate at 20 deg C
        !!    thgra       |none          |temperature adjustment factor for local algal growth rate
        rch_hNH4(ii) = 0.
        rch_hNH4(ii) = nh3con + (ww - xx + yy - zz) * thour         !!    thour       |none          |flow duration (fraction of hr)
        if (rch_hNH4(ii) < 0.) rch_hNH4(ii) = 0.

        !! calculate concentration of nitrite at end of day
        !! QUAL2E section 3.3.3 equation III-19
        yy = 0.
        zz = 0.
        yy = Theta(bc1mod,thbc1,wtmp) * nh3con        !!    bc1mod      |1/day         |rate constant for biological oxidation of NH3 to NO2 modified to reflect impact of low oxygen concentration
        !!    thbc1       |none          |temperature adjustment factor for local biological oxidation of NH3 to NO2
        zz = Theta(bc2mod,thbc2,wtmp) * no2con        !!    bc2mod      |1/day         |rate constant for biological oxidation of NO2 to NO3 modified to reflect impact of low oxygen concentration
        !!    thbc2       |none          |temperature adjustment factor for local biological oxidation of NO2 to NO3
        rch_hNO2(ii) = 0.
        rch_hNO2(ii) = no2con + (yy - zz) * thour       !!    thour       |none          |flow duration (fraction of hr)
        if (rch_hNO2(ii) < 0.) rch_hNO2(ii) = 0.

        !! calculate nitrate concentration at end of day
        !! QUAL2E section 3.3.4 equation III-20
        yy = 0.
        zz = 0.
        yy = Theta(bc2mod,thbc2,wtmp) * no2con        !!    bc2mod      |1/day         |rate constant for biological oxidation of NO2 to NO3 modified to reflect impact of low oxygen concentration       
        !!    thbc2       |none          |temperature adjustment factor for local biological oxidation of NO2 to NO3
        zz = (1. - f1) * ai1 * algcon * Theta(gra,thgra,wtmp)           !!    ai1              |mg N/mg alg   |fraction of algal biomass that is N
        !!    f1          |none          |fraction of algal nitrogen uptake from ammonia pool
        !!    gra         |1/hr          |local algal growth rate at 20 deg C
        !!    thgra       |none          |temperature adjustment factor for local algal growth rate
        rch_hNO3(ii) = 0.
        rch_hNO3(ii) = no3con + (yy - zz) * thour           !!    thour       |none          |flow duration (fraction of hr)
        if (rch_hNO3(ii) < 0.) rch_hNO3(ii) = 0.
!! end nitrogen calculations

!! phosphorus calculations
        !! calculate organic phosphorus concentration at end of
        !! day QUAL2E section 3.3.6 equation III-24
        xx = 0.
        yy = 0.
        zz = 0.
        xx = ai2 * Theta(rhoq,thrho,wtmp) * algcon          !!    ai2              |mg P/mg alg   |fraction of algal biomass that is P
        !!    rhoq             |1/hr          |algal respiration rate at 20 deg C
        !!    thrho       |none          |temperature adjustment factor for local algal respiration rate
        yy = Theta(bc4(jrch),thbc4,wtmp) * orgpcon          !!    bc4(:)           |1/hr          |rate constant for the decay of organic P to dissolved P in reach at 20 deg C
        !!    thbc4       |none          |temperature adjustment factor for local decay of organic P to dissolved P
        zz = Theta(rs5(jrch),thrs5,wtmp) * orgpcon          !!    rs5(:)           |1/hr          |organic phosphorus settling rate in reach at 20 deg C
        !!    thrs5       |none          |temperature adjustment factor for local organic P settling rate
        rch_hOrgP(ii) = 0.
        rch_hOrgP(ii) = orgpcon + (xx - yy - zz) * thour        !!    thour       |none          |flow duration (fraction of hr)
        if (rch_hOrgP(ii) < 0.) rch_hOrgP(ii) = 0.

        !! calculate dissolved phosphorus concentration at end
        !! of day QUAL2E section 3.4.2 equation III-25
        xx = 0.
        yy = 0.
        zz = 0.
        xx = Theta(bc4(jrch),thbc4,wtmp) * orgpcon                      !!    bc4(:)           |1/hr          |rate constant for the decay of organic P to dissolved P in reach at 20 deg C
        !!    thbc4       |none          |temperature adjustment factor for local decay of organic P to dissolved P
        yy = Theta(rs2(jrch),thrs2,wtmp) / (hdepth(ii) * 1000.)         !!    rs2(:)           |(mg disP-P)/  |benthos source rate for dissolved P ((m**2)*hr) |in reach at 20 deg C
        !!    thrs2       |none          |temperature adjustment factor for local benthos source rate for dissolved phosphorus
        zz = ai2 * Theta(gra,thgra,wtmp) * algcon                       !!    ai2              |mg P/mg alg   |fraction of algal biomass that is P
        !!    gra         |1/hr          |local algal growth rate at 20 deg C
        !!    thgra       |none          |temperature adjustment factor for local algal growth rate
        rch_hSolP(ii) = 0.
        rch_hSolP(ii) = solpcon + (xx + yy - zz) * thour                !!    thour       |none          |flow duration (fraction of hr)
        if (rch_hSolP(ii) < 0.) rch_hSolP(ii) = 0.
!! end phosphorus calculations

      else
        !! all water quality variables set to zero when no flow
        algin = 0.0
        chlin = 0.0
        orgnin = 0.0
        ammoin = 0.0
        nitritin = 0.0
        nitratin = 0.0
        orgpin = 0.0
        dispin = 0.0
        cbodin = 0.0
        disoxin = 0.0
        rch_hAlg(ii) = 0.0
        rch_hChla(ii) = 0.0
        rch_hOrgN(ii) = 0.0
        rch_hNH4(ii) = 0.0
        rch_hNO2(ii) = 0.0
        rch_hNO3(ii) = 0.0
        rch_hOrgP(ii) = 0.0
        rch_hSolP(ii) = 0.0
        rch_hCBOD(ii) = 0.0
        rch_hDOX(ii) = 0.0
        soxy = 0.0
      endif
        if (rch_hAlg(ii) < 1.e-6) rch_hAlg(ii) = 0.0
        if (rch_hChla(ii) < 1.e-6) rch_hChla(ii) = 0.0
        if (rch_hOrgN(ii) < 1.e-6) rch_hOrgN(ii) = 0.0
        if (rch_hNH4(ii) < 1.e-6) rch_hNH4(ii) = 0.0
        if (rch_hNO2(ii) < 1.e-6) rch_hNO2(ii) = 0.0
        if (rch_hNO3(ii) < 1.e-6) rch_hNO3(ii) = 0.0
        if (rch_hOrgP(ii) < 1.e-6) rch_hOrgP(ii) = 0.0
        if (rch_hSolP(ii) < 1.e-6) rch_hSolP(ii) = 0.0
        if (rch_hCBOD(ii) < 1.e-6) rch_hCBOD(ii) = 0.0
        if (rch_hDOX(ii) < 1.e-6) rch_hDOX(ii) = 0.0
        if (soxy < 1.e-6) soxy = 0.0

      end do
!! end hourly loop

!! set end of day concentrations
      rch_Alg(jrch) = rch_hAlg(nstep_rch)
      rch_Chla(jrch) = rch_hChla(nstep_rch)
      rch_OrgN(jrch) = rch_hOrgN(nstep_rch)
      rch_NH4(jrch) = rch_hNH4(nstep_rch)
      rch_NO2(jrch) = rch_hNO2(nstep_rch)
      rch_NO3(jrch) = rch_hNO3(nstep_rch)
      rch_OrgP(jrch) = rch_hOrgP(nstep_rch)
      rch_SolP(jrch) = rch_hSolP(nstep_rch)
      rch_CBOD(jrch) = rch_hCBOD(nstep_rch)
      rch_DOX(jrch) = rch_hDOX(nstep_rch)

      if (rch_Alg(jrch) < 1.e-6) rch_Alg(jrch) = 0.0
      if (rch_Chla(jrch) < 1.e-6) rch_Chla(jrch) = 0.0
      if (rch_OrgN(jrch) < 1.e-6) rch_OrgN(jrch) = 0.0
      if (rch_NH4(jrch) < 1.e-6) rch_NH4(jrch) = 0.0
      if (rch_NO2(jrch) < 1.e-6) rch_NO2(jrch) = 0.0
      if (rch_OrgP(jrch) < 1.e-6) rch_OrgP(jrch) = 0.0
      if (rch_SolP(jrch) < 1.e-6) rch_SolP(jrch) = 0.0
      if (rch_CBOD(jrch) < 1.e-6) rch_CBOD(jrch) = 0.0
      if (rch_DOX(jrch) < 1.e-6) rch_DOX(jrch) = 0.0

      return
      end