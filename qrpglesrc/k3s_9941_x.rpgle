      *****************************************************************
     h copyright('(C) Copyright 1996 - 2006 King III Solutions, Inc.  +
     h Rel 4.37 2006-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2006 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_9930
      **   Type: ILE RPG Program
      **   Desc: Control vary text line 3030/31
      **
      **‚  Compile with K3S_ACTG_5 activation group!
      **
      *****************************************************************
      **
      **  This program will receive and build vary tetxt line.
      **
      *****************************************************************
      **
      **  Line 1 to show: DC on hand, Stocking Level, Vendor on order,
      **                  Car count ON/OFF, Supplier Order Cycle
      **
      **  Lines 3 & 4 to show 1 year's worth of how many days in each
      **              month the DC was out of stock
      **
      **  *-- 2021 Fix --*/
      **      Note: I am putting in a fix for 2021 that can be taken out
      **            in the future once we reach year 2022.
      **            Once we have over a year of back screen data, line 4
      **            will show a rolling 12 months. Until we have a full
      **            year I want to blank out the buckets we don't have
      **            data for to make it prettier for display
      **
      *****************************************************************
      **
      **  2022-01-28 King3 - Display Vintage
      **
      *****************************************************************

     fk_productaif   e           k disk    infds(infds) infsr(k3s_infsr)
     f                                     usropn
      * products by locn, supplier, sub supplier, prod

     fk_suplieraif   e           k disk

     fk_prodhisaif   e           k disk

     d comp            s                   like(pr_comp)
     d buyr            s                   like(pr_buyr)
     d locn            s                   like(pr_locn)
     d supl            s                   like(pr_supl)
     d suplsub         s                   like(pr_suplsub)
     d prod            s                   like(pr_prod)
     d altsrce         s                   like(pr_altsrce)
      ** History type begin
     d histype         s                   like(pr_altsrce)
      ** History type end
     d R6_user         s             10a
     d statusa         s              5a
     d carcount        s              3a
     d year            s                   like(ph_year)
     d findyear        s                   like(ph_year)
     d this_year       s                   like(ph_year)
     d this_yearD      s              4
     d last_year       s                   like(ph_year)
     d last_yearD      s              4
     d cur_year        s              4s 0
     d cur_yearD       s              2
     d lst_year        s              4s 0
     d lst_yearD       s              2
     d Jan_yearD       s              2
     d Feb_yearD       s              2
     d Mar_yearD       s              2
     d Apr_yearD       s              2
     d May_yearD       s              2
     d Jun_yearD       s              2
     d Jul_yearD       s              2
     d Aug_yearD       s              2
     d Sep_yearD       s              2
     d Oct_yearD       s              2
     d Nov_yearD       s              2
     d Dec_yearD       s              2
     d cur_month       s              2s 0
     d ptd_recptD      s              7
     d blanks14        s             14
     d bucket01_1      s              7
     d bucket02_1      s              7
     d bucket03_1      s              7
     d bucket04_1      s              7
     d bucket05_1      s              7
     d bucket06_1      s              7
     d bucket07_1      s              7
     d bucket08_1      s              7
     d bucket09_1      s              7
     d bucket10_1      s              7
     d bucket11_1      s              7
     d bucket12_1      s              7
     d bucket01_2      s              7
     d bucket02_2      s              7
     d bucket03_2      s              7
     d bucket04_2      s              7
     d bucket05_2      s              7
     d bucket06_2      s              7
     d bucket07_2      s              7
     d bucket08_2      s              7
     d bucket09_2      s              7
     d bucket10_2      s              7
     d bucket11_2      s              7
     d bucket12_2      s              7
     d thisyear01      s                   like(ph_per01)
     d thisyear02      s                   like(ph_per01)
     d thisyear03      s                   like(ph_per01)
     d thisyear04      s                   like(ph_per01)
     d thisyear05      s                   like(ph_per01)
     d thisyear06      s                   like(ph_per01)
     d thisyear07      s                   like(ph_per01)
     d thisyear08      s                   like(ph_per01)
     d thisyear09      s                   like(ph_per01)
     d thisyear10      s                   like(ph_per01)
     d thisyear11      s                   like(ph_per01)
     d thisyear12      s                   like(ph_per01)
     d lastyear01      s                   like(ph_per01)
     d lastyear02      s                   like(ph_per01)
     d lastyear03      s                   like(ph_per01)
     d lastyear04      s                   like(ph_per01)
     d lastyear05      s                   like(ph_per01)
     d lastyear06      s                   like(ph_per01)
     d lastyear07      s                   like(ph_per01)
     d lastyear08      s                   like(ph_per01)
     d lastyear09      s                   like(ph_per01)
     d lastyear10      s                   like(ph_per01)
     d lastyear11      s                   like(ph_per01)
     d lastyear12      s                   like(ph_per01)
     d zz_varytxt      s            130a
     d show1_1st       s             26a   inz('X-------------------------')
     d show1_2nd       s             26a   inz('---------------------- thi')
     d show1_3rd       s             26a   inz('s line is under your contr')
     d show1_4th       s             26a   inz('ol -----------------------')
     d show1_5th       s             26a   inz('-------------------------X')
     d zz_varytx2      s            130a
     d show2_1st       s             26a   inz('X-------------------------')
     d show2_2nd       s             26a   inz('---------------------- thi')
     d show2_3rd       s             26a   inz('s is the 2nd line under yo')
     d show2_4th       s             26a   inz('ur control ---------------')
     d show2_5th       s             26a   inz('-------------------------X')
     d zz_varytx3      s            130a
     d show3_1st       s             26a   inz('X-------------------------')
     d show3_2nd       s             26a   inz('---------------------- thi')
     d show3_3rd       s             26a   inz('s is the 3rd line under yo')
     d show3_4th       s             26a   inz('ur control ---------------')
     d show3_5th       s             26a   inz('-------------------------X')
     d zz_varytx4      s            130a
     d show4_1st       s             26a   inz('X-------------------------')
     d show4_2nd       s             26a   inz('---------------------- thi')
     d show4_3rd       s             26a   inz('s is the 4th line under yo')
     d show4_4th       s             26a   inz('ur control ---------------')
     d show4_5th       s             26a   inz('-------------------------X')
     d infds           ds
     d status            *status
      * parameters passed to program
     c     *entry        plist
     c                   parm                    comp
     c                   parm                    buyr
     c                   parm                    locn
     c                   parm                    supl
     c                   parm                    suplsub
     c                   parm                    prod
     c                   parm                    altsrce
      ** History type begin
     c***                parm                    histype
      ** History type end
     c                   parm                    R6_user
     c                   parm                    zz_varytxt
     c                   parm                    zz_varytx2
     c                   parm                    zz_varytx3
     c                   parm                    zz_varytx4

      * key list for product file
     c     px_key        klist
     c                   kfld                    comp
     c                   kfld                    locn
     c                   kfld                    supl
     c                   kfld                    suplsub
     c                   kfld                    prod

     c     sp_key        klist
     c                   kfld                    pr_comp
     c                   kfld                    pr_locn
     c                   kfld                    pr_supl
     c                   kfld                    pr_suplsub

     c     ph_key        klist
     c                   kfld                    pr_comp
     c                   kfld                    pr_locn
     c                   kfld                    pr_suplusr
     c                   kfld                    pr_suplusb
     c                   kfld                    pr_prod
     c                   kfld                    year
     c                   kfld                    pr_forcint
     c                   kfld                    histype

     c                   eval      *in28 = *off
     c                   open      k_producta                           28
     c                   exsr      k3s_infsr
     c                   if        *in28 = *off
     c     px_key        chain(n)  rk_product                         2728
     c                   exsr      k3s_infsr
     c                   if        *in27 = *off and
     c                             *in28 = *off

     c                   eval      this_year  = pr_forcyr
     c                   eval      this_yearD = %trim(%editc(this_year:'3'))
     c                   eval      last_year  = pr_forcyr - 1
     c                   eval      last_yearD = %trim(%editc(last_year:'3'))

     c     sp_key        chain(n)  rk_suplier

     c** Develop 12 headings for MonthYear
     c                   exsr      $_MonthYear

     c** Fill 12 buckets of data with PO Receipts histype = 1
     c                   exsr      $_clear
     c                   exsr      $_buckets_1

     c** Fill 12 buckets of data with Demand histype = 0
     c                   exsr      $_clear
     c                   exsr      $_buckets_2

     c** Develop PTD PO Receipts
     c                   exsr      $_ptd_recpt

     c                   eval      zz_varytxt = *blanks
     c                   eval      zz_varytx2 = *blanks
     c                   eval      zz_varytx3 = *blanks
     c                   eval      zz_varytx4 = *blanks

     C*********          eval      zz_varytxt = ' PTD PO Receipts: ' +
     C*********                           ptd_recptD

     c*******            eval      zz_varytx2 = blanks14 + '    ' +
     c                   eval      zz_varytx2 = '     Vintage:'  +
     c                             %subst(pr_group5:1:5)    +
     c                             ' Jan' + Jan_yearD + ' ' +
     c                             ' Feb' + Feb_yearD + ' ' +
     c                             ' Mar' + Mar_yearD + ' ' +
     c                             ' Apr' + Apr_yearD + ' ' +
     c                             ' May' + May_yearD + ' ' +
     c                             ' Jun' + Jun_yearD + ' ' +
     c                             ' Jul' + Jul_yearD + ' ' +
     c                             ' Aug' + Aug_yearD + ' ' +
     c                             ' Sep' + Sep_yearD + ' ' +
     c                             ' Oct' + Oct_yearD + ' ' +
     c                             ' Nov' + Nov_yearD + ' ' +
     c                             ' Dec' + Dec_yearD

     c***                eval      zz_varytx3 = '2020 Receipts' +
     c                   eval      zz_varytx3 = '    '     + ' Demand      ' +
     c                                 bucket01_2 + bucket02_2 +
     c                                 bucket03_2 + bucket04_2 +
     c                                 bucket05_2 + bucket06_2 +
     c                                 bucket07_2 + bucket08_2 +
     c                                 bucket09_2 + bucket10_2 +
     c                                 bucket11_2 + bucket12_2

     c*****              eval      zz_varytx4 = '2021 Receipts' +
     c                   eval      zz_varytx4 = '    '     + ' PO Receipts ' +
     c                                 bucket01_1 + bucket02_1 +
     c                                 bucket03_1 + bucket04_1 +
     c                                 bucket05_1 + bucket06_1 +
     c                                 bucket07_1 + bucket08_1 +
     c                                 bucket09_1 + bucket10_1 +
     c                                 bucket11_1 + bucket12_1

     c                   else
     c                   endif
     c                   endif

     ‚*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     ‚*  If text is set to default, get ours:
     C***                if        %subst(zz_varytxt:1:26) = show1_1st
     C***                call      'CLK3S202'
     C***                parm                    supl
     C***                parm                    prod
     C***                parm                    zz_varytxt
     C***                parm                    zz_varytx2
     C***                endif
     ‚*
     ‚*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     c                   return

      * //////////////////////////////////////////////// Edit PB numbers


     c     k3s_infsr     begsr
     c                   if        status > 00000
     c                   if        status = 01215
     c                   eval      *in28 = *off
     c                   endif
     c                   if        status = 00011 or
     c                             status = 00012
     c                   eval      zz_varytxt = *blanks
     c                   eval      zz_varytxt = 'No information found for this +
     c                                          product or supplier  '
     c                   endif
     c                   if        status = 01011 or
     c                             status = 01211 or
     c                             status = 01216 or
     c                             status = 01217 or
     c                             status = 01299
     c                   movel     status        statusa
     c                   eval      zz_varytxt = *blanks
     c                   eval      zz_varytxt = %subst('File error' : 1) + ' ' +
     c                                          statusa + ' ' +
     c                                          'contact your IS department'
     c                   endif
     c                   if        status = 01218
     c                   eval      zz_varytxt = *blanks
     c                   eval      zz_varytxt = 'File error record locked by an+
     c                                          other job, try again '
     c                   endif
     c                   endif
     c                   endsr

      *****************************************************************

     c     $_clear       begsr

     c                   clear                   thisyear01
     c                   clear                   thisyear02
     c                   clear                   thisyear03
     c                   clear                   thisyear04
     c                   clear                   thisyear05
     c                   clear                   thisyear06
     c                   clear                   thisyear07
     c                   clear                   thisyear08
     c                   clear                   thisyear09
     c                   clear                   thisyear10
     c                   clear                   thisyear11
     c                   clear                   thisyear12

     c                   clear                   lastyear01
     c                   clear                   lastyear02
     c                   clear                   lastyear03
     c                   clear                   lastyear04
     c                   clear                   lastyear05
     c                   clear                   lastyear06
     c                   clear                   lastyear07
     c                   clear                   lastyear08
     c                   clear                   lastyear09
     c                   clear                   lastyear10
     c                   clear                   lastyear11
     c                   clear                   lastyear12

     c                   endsr

      *****************************************************************

      ** PO Receipts

     c     $_buckets_1   begsr

      **    this year
     c                   eval      histype = 1
     c                   eval      year = pr_forcyr
     c     ph_key        chain     rk_prodhis
     c                   if        %found(k_prodhisa)
     c                   eval      thisyear01 = ph_per01
     c                   eval      thisyear02 = ph_per02
     c                   eval      thisyear03 = ph_per03
     c                   eval      thisyear04 = ph_per04
     c                   eval      thisyear05 = ph_per05
     c                   eval      thisyear06 = ph_per06
     c                   eval      thisyear07 = ph_per07
     c                   eval      thisyear08 = ph_per08
     c                   eval      thisyear09 = ph_per09
     c                   eval      thisyear10 = ph_per10
     c                   eval      thisyear11 = ph_per11
     c                   eval      thisyear12 = ph_per12
     c                   endif

      **    last year
     c                   eval      year = pr_forcyr - 1
     c     ph_key        chain     rk_prodhis
     c                   if        %found(k_prodhisa)
     c                   eval      lastyear01 = ph_per01
     c                   eval      lastyear02 = ph_per02
     c                   eval      lastyear03 = ph_per03
     c                   eval      lastyear04 = ph_per04
     c                   eval      lastyear05 = ph_per05
     c                   eval      lastyear06 = ph_per06
     c                   eval      lastyear07 = ph_per07
     c                   eval      lastyear08 = ph_per08
     c                   eval      lastyear09 = ph_per09
     c                   eval      lastyear10 = ph_per10
     c                   eval      lastyear11 = ph_per11
     c                   eval      lastyear12 = ph_per12
     c                   endif

     c                   if        pr_forcper = 1
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(lastyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(lastyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(lastyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(lastyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 2
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(lastyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(lastyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(lastyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 3
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(lastyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(lastyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 4
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(lastyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 5
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 6
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 7
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 8
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 9
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(thisyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 10
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(thisyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(thisyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 11
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(thisyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(thisyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(thisyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 12
     c                   eval      bucket01_1= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_1= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_1= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_1= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_1= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_1= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_1= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_1= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_1= %trimr(%editc(thisyear09:'3'))
     c                   eval      bucket10_1= %trimr(%editc(thisyear10:'3'))
     c                   eval      bucket11_1= %trimr(%editc(thisyear11:'3'))
     c                   eval      bucket12_1= %trimr(%editc(thisyear12:'3'))
     c                   endif

     c                   endsr

      *****************************************************************
      *****************************************************************

      ** Demand

     c     $_buckets_2   begsr

      **    this year
     c                   eval      histype = 0
     c                   eval      year = pr_forcyr
     c     ph_key        chain     rk_prodhis
     c                   if        %found(k_prodhisa)
     c                   eval      thisyear01 = ph_per01
     c                   eval      thisyear02 = ph_per02
     c                   eval      thisyear03 = ph_per03
     c                   eval      thisyear04 = ph_per04
     c                   eval      thisyear05 = ph_per05
     c                   eval      thisyear06 = ph_per06
     c                   eval      thisyear07 = ph_per07
     c                   eval      thisyear08 = ph_per08
     c                   eval      thisyear09 = ph_per09
     c                   eval      thisyear10 = ph_per10
     c                   eval      thisyear11 = ph_per11
     c                   eval      thisyear12 = ph_per12

      **    get PTD answer for correct month
     c                   if        pr_forcper = 1
     c                   eval      thisyear01 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 2
     c                   eval      thisyear02 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 3
     c                   eval      thisyear03 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 4
     c                   eval      thisyear04 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 5
     c                   eval      thisyear05 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 6
     c                   eval      thisyear06 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 7
     c                   eval      thisyear07 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 8
     c                   eval      thisyear08 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 9
     c                   eval      thisyear09 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 10
     c                   eval      thisyear10 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 11
     c                   eval      thisyear11 = pr_accdem
     c                   endif
     c                   if        pr_forcper = 12
     c                   eval      thisyear12 = pr_accdem
     c                   endif

     c                   endif

      **    last year
     c                   eval      year = pr_forcyr - 1
     c     ph_key        chain     rk_prodhis
     c                   if        %found(k_prodhisa)
     c                   eval      lastyear01 = ph_per01
     c                   eval      lastyear02 = ph_per02
     c                   eval      lastyear03 = ph_per03
     c                   eval      lastyear04 = ph_per04
     c                   eval      lastyear05 = ph_per05
     c                   eval      lastyear06 = ph_per06
     c                   eval      lastyear07 = ph_per07
     c                   eval      lastyear08 = ph_per08
     c                   eval      lastyear09 = ph_per09
     c                   eval      lastyear10 = ph_per10
     c                   eval      lastyear11 = ph_per11
     c                   eval      lastyear12 = ph_per12
     c                   endif

     c                   if        pr_forcper = 1
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(lastyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(lastyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(lastyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(lastyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 2
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(lastyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(lastyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(lastyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 3
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(lastyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(lastyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 4
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(lastyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 5
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(lastyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 6
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(lastyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 7
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(lastyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 8
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(lastyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 9
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(thisyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(lastyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 10
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(thisyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(thisyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(lastyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 11
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(thisyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(thisyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(thisyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(lastyear12:'3'))
     c                   endif

     c                   if        pr_forcper = 12
     c                   eval      bucket01_2= %trimr(%editc(thisyear01:'3'))
     c                   eval      bucket02_2= %trimr(%editc(thisyear02:'3'))
     c                   eval      bucket03_2= %trimr(%editc(thisyear03:'3'))
     c                   eval      bucket04_2= %trimr(%editc(thisyear04:'3'))
     c                   eval      bucket05_2= %trimr(%editc(thisyear05:'3'))
     c                   eval      bucket06_2= %trimr(%editc(thisyear06:'3'))
     c                   eval      bucket07_2= %trimr(%editc(thisyear07:'3'))
     c                   eval      bucket08_2= %trimr(%editc(thisyear08:'3'))
     c                   eval      bucket09_2= %trimr(%editc(thisyear09:'3'))
     c                   eval      bucket10_2= %trimr(%editc(thisyear10:'3'))
     c                   eval      bucket11_2= %trimr(%editc(thisyear11:'3'))
     c                   eval      bucket12_2= %trimr(%editc(thisyear12:'3'))
     c                   endif


     c                   endsr

      *****************************************************************

     c     $_ptd_recpt   begsr

     c                   eval      histype = 1
     c                   eval      year = pr_forcyr
     c     ph_key        chain     rk_prodhis
     c                   if        %found(k_prodhisa)

     c                   select
     c                   when      pr_forcper = 1
     c                   eval      ptd_recptD= %trim(%editc(ph_per01:'3'))
     c                   when      pr_forcper = 2
     c                   eval      ptd_recptD= %trim(%editc(ph_per02:'3'))
     c                   when      pr_forcper = 3
     c                   eval      ptd_recptD= %trim(%editc(ph_per03:'3'))
     c                   when      pr_forcper = 4
     c                   eval      ptd_recptD= %trim(%editc(ph_per04:'3'))
     c                   when      pr_forcper = 5
     c                   eval      ptd_recptD= %trim(%editc(ph_per05:'3'))
     c                   when      pr_forcper = 6
     c                   eval      ptd_recptD= %trim(%editc(ph_per06:'3'))
     c                   when      pr_forcper = 7
     c                   eval      ptd_recptD= %trim(%editc(ph_per07:'3'))
     c                   when      pr_forcper = 8
     c                   eval      ptd_recptD= %trim(%editc(ph_per08:'3'))
     c                   when      pr_forcper = 9
     c                   eval      ptd_recptD= %trim(%editc(ph_per09:'3'))
     c                   when      pr_forcper = 10
     c                   eval      ptd_recptD= %trim(%editc(ph_per10:'3'))
     c                   when      pr_forcper = 11
     c                   eval      ptd_recptD= %trim(%editc(ph_per11:'3'))
     c                   when      pr_forcper = 12
     c                   eval      ptd_recptD= %trim(%editc(ph_per12:'3'))
     c                   endsl

     c                   endif
     c                   endsr

      *****************************************************************

     c     $_MonthYear   begsr

     c                   eval      cur_year   = %subdt(%date():*years)
     c                   eval      cur_yearD  = %subst(%char(cur_year):03:02)
     c                   eval      lst_year   = cur_year - 1
     c                   eval      lst_yearD  = %subst(%char(lst_year):03:02)
     c                   eval      cur_month  = %subdt(%date():*months)

     c                   if        cur_month  = 01
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = lst_yearD
     c                   eval      Mar_yearD  = lst_yearD
     c                   eval      Apr_yearD  = lst_yearD
     c                   eval      May_yearD  = lst_yearD
     c                   eval      Jun_yearD  = lst_yearD
     c                   eval      Jul_yearD  = lst_yearD
     c                   eval      Aug_yearD  = lst_yearD
     c                   eval      Sep_yearD  = lst_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 02
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = lst_yearD
     c                   eval      Apr_yearD  = lst_yearD
     c                   eval      May_yearD  = lst_yearD
     c                   eval      Jun_yearD  = lst_yearD
     c                   eval      Jul_yearD  = lst_yearD
     c                   eval      Aug_yearD  = lst_yearD
     c                   eval      Sep_yearD  = lst_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 03
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = lst_yearD
     c                   eval      May_yearD  = lst_yearD
     c                   eval      Jun_yearD  = lst_yearD
     c                   eval      Jul_yearD  = lst_yearD
     c                   eval      Aug_yearD  = lst_yearD
     c                   eval      Sep_yearD  = lst_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 04
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = lst_yearD
     c                   eval      Jun_yearD  = lst_yearD
     c                   eval      Jul_yearD  = lst_yearD
     c                   eval      Aug_yearD  = lst_yearD
     c                   eval      Sep_yearD  = lst_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 05
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = cur_yearD
     c                   eval      Jun_yearD  = lst_yearD
     c                   eval      Jul_yearD  = lst_yearD
     c                   eval      Aug_yearD  = lst_yearD
     c                   eval      Sep_yearD  = lst_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 06
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = cur_yearD
     c                   eval      Jun_yearD  = cur_yearD
     c                   eval      Jul_yearD  = lst_yearD
     c                   eval      Aug_yearD  = lst_yearD
     c                   eval      Sep_yearD  = lst_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 07
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = cur_yearD
     c                   eval      Jun_yearD  = cur_yearD
     c                   eval      Jul_yearD  = cur_yearD
     c                   eval      Aug_yearD  = lst_yearD
     c                   eval      Sep_yearD  = lst_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 08
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = cur_yearD
     c                   eval      Jun_yearD  = cur_yearD
     c                   eval      Jul_yearD  = cur_yearD
     c                   eval      Aug_yearD  = cur_yearD
     c                   eval      Sep_yearD  = lst_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 09
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = cur_yearD
     c                   eval      Jun_yearD  = cur_yearD
     c                   eval      Jul_yearD  = cur_yearD
     c                   eval      Aug_yearD  = cur_yearD
     c                   eval      Sep_yearD  = cur_yearD
     c                   eval      Oct_yearD  = lst_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 10
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = cur_yearD
     c                   eval      Jun_yearD  = cur_yearD
     c                   eval      Jul_yearD  = cur_yearD
     c                   eval      Aug_yearD  = cur_yearD
     c                   eval      Sep_yearD  = cur_yearD
     c                   eval      Oct_yearD  = cur_yearD
     c                   eval      Nov_yearD  = lst_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 11
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = cur_yearD
     c                   eval      Jun_yearD  = cur_yearD
     c                   eval      Jul_yearD  = cur_yearD
     c                   eval      Aug_yearD  = cur_yearD
     c                   eval      Sep_yearD  = cur_yearD
     c                   eval      Oct_yearD  = cur_yearD
     c                   eval      Nov_yearD  = cur_yearD
     c                   eval      Dec_yearD  = lst_yearD
     c                   endif

     c                   if        cur_month  = 12
     c                   eval      Jan_yearD  = cur_yearD
     c                   eval      Feb_yearD  = cur_yearD
     c                   eval      Mar_yearD  = cur_yearD
     c                   eval      Apr_yearD  = cur_yearD
     c                   eval      May_yearD  = cur_yearD
     c                   eval      Jun_yearD  = cur_yearD
     c                   eval      Jul_yearD  = cur_yearD
     c                   eval      Aug_yearD  = cur_yearD
     c                   eval      Sep_yearD  = cur_yearD
     c                   eval      Oct_yearD  = cur_yearD
     c                   eval      Nov_yearD  = cur_yearD
     c                   eval      Dec_yearD  = cur_yearD
     c                   endif

     c                   endsr
