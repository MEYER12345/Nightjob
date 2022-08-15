      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2008 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
     h NOMAIN
     h OPTION(*NODEBUGIO)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
       //****************************************************************
       //  Subprocedure: K3S_Calc_Safety
       //  Desc: Calculate Safety Stock component values
       //  (From K3S_C090 and K3S_C091)
       //****************************************************************
     p K3S_Calc_Safety...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  repcary                       3  3 const
     d  linecst                       5  2 const
     d  orcycle                       3  0 const
     d  forcint                       3  0 const
     d  buymult                       7  0 const
     d  minqty                        7  0 const
     d  costeac                      11  4 const
     d  forcast                       9  2 const
     d  fordevp                       3  3 const
     d  service                       3  3 const
     d  leadtm                        3  0 const
     d  leadtmv                       3  3 const
     d  formeth                       1  0 const
     d  sstimef                       9  3
     d  otimfac                       3  0
     d  devtime                       7  3
     d  intrval                       5  0
     d  meandevary                    5  4 dim(71) ascend
     d  strddevary                    5  4 dim(37) ascend
      * ------------------------ ---------------------------local variables
     d eoq             s             15  3
     d minord          s             15  3
     d eof             s              3  0
     d iof             s              9  3
     d gwork           s             11  4
     d g               s              5  4
     d k               s              5  4
     d x               s              3  0

      /free
       /////////////////////////////////// calculate safety stock components

       // --------------------------------------------------------------------

       // calculate devtime

       eval(h)   devtime = (((((orcycle +leadtm)*364)
                           / forcint)
                           * ((fordevp) **2)
                           + ((leadtm * leadtmv) **2)))
                           ** .5;

       // devtime must not be less than 1

       if devtime < 1;
          devtime = 1;
       endif;

       // --------------------------------------------------------------------

       // calculate economic order quantity

       //      for very inexpensive products
       if repcary * costeac = 0;
          eoq = (forcast * forcint) ** .5;

       //      for most products
       else;
          eval(h)   eoq = ( (2 * (forcast * forcint)
                                  *linecst)
                               / (repcary * costeac)
                                      ) ** .5;


       endif;

       // eoq must be at least one order cycle

       eval(h)   minord = (forcast * forcint *
                             orcycle) / 364;

       if eoq < minord;
          eoq = minord;
       endif;

       // eoq must not be 0

       if eoq = 0;
          eoq = 1;
       endif;

       // --------------------------------------------------------------------

       // calculate economic order interval

       if forcast = 0;
          otimfac = 364;
       else;

          eval(h)   iof = (forcast * forcint)
                               / eoq;

       // calculate economic order interval in days (unadjusted interval)
       //   this value is used to calcuate max days and units for soq's
       //              when building orders,
       //   and also used in order cycle analysis to help determine the
       //              quantity bought for different order strategies

          if 364/iof <= 99999;
             eval(h)   intrval = 364/iof;
          else;
             intrval = 364;
             otimfac = 364;
          endif;

       // eoq must at least purchase buying multiple

          if eoq < buymult;
             eoq = buymult;
          endif;

       // eoq must at least purchase minimum quantity

          if eoq < minqty;
             eoq = minqty;
          endif;

       endif;

       //--------------------------------------------------------------------

       //  calculate economic order frequency

       eval(h) iof = (forcast * forcint)
                        / eoq;

       // must buy at least once a year

       if iof < 1;
          iof = 1;
       endif;

       eval(h) otimfac = 364 / iof;

       eval(h) eof = 364 / otimfac;

       // --------------------------------------------------------------------

       // calculate safety stock time or in days

       eval(h) gwork = (((1 - service) * 364)
                        / devtime) / eof;

       // ----- mean absolute deviation ------------------------- begin
       if formeth = 0;
          if gwork > .4969;
             clear k;

          else;
             if gwork < 0;
                k = 4.1;

             else;

                k = 0;
                g = gwork;

                x = 1;
                x = %lookuple(g:meandevary);

                k = 4.1 - (x * .1);

             endif;
          endif;
       endif;
       // ----- mean absolute deviation ------------------------- end

       // ----- standard deviation ------------------------------ begin
       if formeth = 1;
          if gwork > .4999;
             clear k;

          else;
             if gwork < 0;
                k = 3.7;

             else;

                k = 0;
                g = gwork;

                x = 1;
                x = %lookuple(g:strddevary);

                k = 3.7 - (x * .1);

             endif;
          endif;
       endif;
       // ----- standard deviation ------------------------------ end

       if (k * devtime) < 900;
          sstimef = (k * devtime);
       else;
          sstimef = 900;
       endif;

      /end-free
     p K3S_Calc_Safety...
     p                 E

       //****************************************************************
       //  Subprocedure: K3S_Calc_SOQ
       //  Desc: Calculate SOQ for product
       //  (From K3S_C120 and K3S_C121)
       //
       //**************************************************************
     p K3S_Calc_SOQ...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  maxunit                       7  0 const                                Order up to level
     d  minunit                       7  0 const                                Order point
     d  qtybaln                       7  0 const                                Quantity balance
     d  minqty                        7  0 const                                Minimum quantity
     d  buymult                       7  0 const                                Buying multiple
     d  convpak                       7  0 const                                Convenience pack
     d  convpkp                       3  1 const                                Convenience pack %
     d  soq                           7  0


      * work fields
     d cnvpk           s              7  0
     d cnvpk1          s              7  0
     d cnvpk2          s              7  0
     d cnvpk3          s              7  0
     d soqw1           s              7  0
     d soqw2           s              7  0

      /free
       //--------------------------------------------------------------------
       //    calculate soq
       soq = maxunit - qtybaln;

       //  test if enough product available
       if soq <= 0;
          soq = 0;
       else;

       // breakpoint test goes here

       // minimum quantity purchased check

          if soq < minqty;
             soq = minqty;
          endif;

       // round soq to buying multiple

          soqw1 = soq / buymult;
          soqw2 = %rem(soq:buymult);

          if soqw2 > 0;

             if soqw1 = 0;
                soqw1 += 1;
             else;
                eval(h) soqw1 = soq / buymult;
             endif;

             soq = soqw1 * buymult;

             soqw2 = soq + qtybaln;

       // make sure quantity is not under order point
             if soqw2 < minunit;
                soq += buymult;
             endif;

          endif;

       // convenience pack test
       //     logic to buy first convenience pack

          if (convpak > 0) and
             (soq < convpak);

             eval(h) cnvpk = convpak * (convpkp *.01);

             if soq >= cnvpk;
                soq = convpak;
             endif;

          endif;

       //     logic to buy after first convenience pack

          if (convpak > 0) and
             (soq > convpak);

             eval(h) cnvpk = convpak * (convpkp *.01);

             cnvpk1 = soq / convpak;
             cnvpk2 = cnvpk1 * convpak;
             cnvpk3 = soq - cnvpk2;

             if (cnvpk3 > 0) and
                (cnvpk3 >= cnvpk);

                cnvpk1 += 1;
                soq = cnvpk1 * convpak;
             endif;

          endif;

       endif;
      /end-free
     p K3S_Calc_Soq...
     p                 E
       //****************************************************************
       //  Subprocedure: K3S_Calc_Usage
       //  Desc: Calculate usage (days into units)
       //  (From K3S_C100 and K3S_C101)
       //
       //****************************************************************
     p K3S_Calc_Usage...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  comp                          1    const
     d  seasonl                      10    const
     d  days                         11  3
     d  forcast                       9  2 const
     d  forcper                       3  0 const
     d  forcint                       3  0 const
     d  longtrm                       3  2 const
     d  dowmap                        1  0
     d  dowper1                       5  2 const
     d  dowper2                       5  2 const
     d  dowper3                       5  2 const
     d  dowper4                       5  2 const
     d  dowper5                       5  2 const
     d  dowper6                       5  2 const
     d  dowper7                       5  2 const
     d  sstper1                       5  2 const
     d  sstper2                       5  2 const
     d  sstper3                       5  2 const
     d  sstper4                       5  2 const
     d  sstper5                       5  2 const
     d  sstper6                       5  2 const
     d  sstper7                       5  2 const
     d  diff12                        3  0 const
     d  diff13                        3  0 const
     d  diff52                        3  0 const
     d  use                          15  3
     d  Ds_flag                       1  0
     d  DayNbr                        1  0
     d  ix                            5s 2 dim(52)
     d  Ds_days                       2  0
     d  Ds_wdis                       5  2 dim(12)
     d  Ds_dlyu                      15  4 dim(12)
     d  Ds_spct                       5  2 dim(12)
     d  Ds_susg                      15  4 dim(12)
     d  Ds_accu                      15  4 dim(12)
     d  Ds_prno                       3  0 dim(12)
     d  Ds_fctr                       5  2 dim(12)
     d  Ds_warn                       1  0

      * work fields used in subprocedure

      * work fields
     d fcst            s             11  3
     d prno            s              3  0
     d time            s             11  3
     d units           s             15  4
     d years           s              4  0
     d u1sttest        s             10
     d j               s              3  0

      * work fields for Weekly Distribution logic
     d WD_unit         s             15  4 dim(7)                               Each day's units
     d WD_safe         s             15  4 dim(7)                               Each day's SS units
     d uDoW            s              1  0                                      Day of week #
     d udo             s              3  0                                      Do loop process

      * work fields for Weekly Distribution logic display results
     d Ds_coun         s              2  0                                      Count days built

      /free
       //--------------------------------------------------------------------

       // no seasonal profile exists for product, and long term trend = 1.00,
       //    and Weekly Distribution logic not being used
       if (seasonl = *blanks) and
          (longtrm = 1.00)    and
          (dowmap  = 0);                   //0 = not weekly

          eval(h)   use = (forcint * forcast *
                             days) / 364;

       else;
       // either a profile exists for product, or long term trend <> 1.00,
       //    or Weekly Distribution logic being used

          fcst = forcast;
          prno = forcper;

       // set time, which is number of days left in the current period
          select;
             when forcint = 12;
                  time = diff12;
             when forcint = 13;
                  time = diff13;
             when forcint = 52;
                  time = diff52;
          endsl;

       // ----------------------------------------------------
       // calculate usage (Weekly Distribution NOT being used)

          if dowmap = 0;

             dow days > 0;

                if time > days;
                   time = days;
                endif;

                eval(h) units = (ix(prno) * forcint *
                                 fcst * time) / 364;

                use += units;

                days -= time;

                if days > 0;

                   eval(h) fcst = fcst * longtrm;
                   eval(h) time = 364/ forcint;

                   prno += 1;
                   if prno > forcint;
                      prno = 1;
                   endif;

                endif;

             enddo;

          endif;

       // ----------------------------------------------------
       // calculate usage (Weekly Distribution IS being used)

          if dowmap = 1;     //weekly = 1

             if Ds_flag = 1;
                Ds_coun = 1;
                if days >= 13;
                   Ds_warn = 1;
                else;
                   Ds_warn = 0;
                endif;
                if days < 13;
                   Ds_days = days;
                else;
                   Ds_days = 12;
                endif;
             endif;


       //      daily usage by day of the week
             eval(h) WD_unit(1) = ((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper1 * .01);
             eval(h) WD_unit(2) = ((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper2 * .01);
             eval(h) WD_unit(3) = ((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper3 * .01);
             eval(h) WD_unit(4) = ((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper4 * .01);
             eval(h) WD_unit(5) = ((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper5 * .01);
             eval(h) WD_unit(6) = ((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper6 * .01);
             eval(h) WD_unit(7) = ((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper7 * .01);

       //      safety stock usage by day of the week
             eval(h) WD_safe(1) = (((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper1 * .01)) * (sstper1 * .01);
             eval(h) WD_safe(2) = (((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper2 * .01)) * (sstper2 * .01);
             eval(h) WD_safe(3) = (((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper3 * .01)) * (sstper3 * .01);
             eval(h) WD_safe(4) = (((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper4 * .01)) * (sstper4 * .01);
             eval(h) WD_safe(5) = (((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper5 * .01)) * (sstper5 * .01);
             eval(h) WD_safe(6) = (((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper6 * .01)) * (sstper6 * .01);
             eval(h) WD_safe(7) = (((ix(prno) *
                     forcint * fcst) / 52) *
                     (dowper7 * .01)) * (sstper7 * .01);

             eval uDoW  = DayNbr;

             dow days > 0;

                 if time > days;
                    time = days;
                 endif;

                 units = 0;
                 udo    = time;
                 For j = 1 to udo;
                    eval(h) units = units +
                            WD_unit(uDoW) +
                            WD_safe(uDoW);

       //   Calculate display results?  only up to 13 days maximum
                    if Ds_flag = 1;
                       if Ds_coun <= Ds_days;
                          Ds_prno(Ds_coun) = prno;
                          Ds_fctr(Ds_coun) = ix(prno);
                          select;
                             when uDoW = 1;
                                  Ds_wdis(Ds_coun) = dowper1;
                                  Ds_spct(Ds_coun) = sstper1;
                             when uDoW = 2;
                                  Ds_wdis(Ds_coun) = dowper2;
                                  Ds_spct(Ds_coun) = sstper2;
                             when uDoW = 3;
                                  Ds_wdis(Ds_coun) = dowper3;
                                  Ds_spct(Ds_coun) = sstper3;
                             when uDoW = 4;
                                  Ds_wdis(Ds_coun) = dowper4;
                                  Ds_spct(Ds_coun) = sstper4;
                             when uDoW = 5;
                                  Ds_wdis(Ds_coun) = dowper5;
                                  Ds_spct(Ds_coun) = sstper5;
                             when uDoW = 6;
                                  Ds_wdis(Ds_coun) = dowper6;
                                  Ds_spct(Ds_coun) = sstper6;
                             when uDoW = 7;
                                  Ds_wdis(Ds_coun) = dowper7;
                                  Ds_spct(Ds_coun) = sstper7;
                          endsl;
                          Ds_dlyu(Ds_coun)= WD_unit(uDoW);
                          Ds_susg(Ds_coun)= WD_safe(uDoW);
                          Ds_accu(Ds_coun)= use + units;
                          Ds_coun += 1;
                       endif;
                    endif;

                    uDoW += 1;
                    if uDoW > 7;
                       uDoW = 1;
                    endif;
                 endfor;

                 use += units;
                 days -= time;

                 if days > 0;

                    eval(h) fcst = fcst * longtrm;
                    eval(h) time = 364/ forcint;

                    prno += 1;
                    if prno > forcint;
                       prno = 1;
                    endif;

       //      daily usage by day of the week
                    eval(h) WD_unit(1) = ((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper1 * .01);
                    eval(h) WD_unit(2) = ((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper2 * .01);
                    eval(h) WD_unit(3) = ((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper3 * .01);
                    eval(h) WD_unit(4) = ((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper4 * .01);
                    eval(h) WD_unit(5) = ((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper5 * .01);
                    eval(h) WD_unit(6) = ((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper6 * .01);
                    eval(h) WD_unit(7) = ((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper7 * .01);

       //      safety stock usage by day of the week
                    eval(h) WD_safe(1) = (((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper1 * .01)) * (sstper1 * .01);
                    eval(h) WD_safe(2) = (((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper2 * .01)) * (sstper2 * .01);
                    eval(h) WD_safe(3) = (((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper3 * .01)) * (sstper3 * .01);
                    eval(h) WD_safe(4) = (((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper4 * .01)) * (sstper4 * .01);
                    eval(h) WD_safe(5) = (((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper5 * .01)) * (sstper5 * .01);
                    eval(h) WD_safe(6) = (((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper6 * .01)) * (sstper6 * .01);
                    eval(h) WD_safe(7) = (((ix(prno) *
                            forcint * fcst) / 52) *
                            (dowper7 * .01)) * (sstper7 * .01);

                 endif;

             enddo;

          endif;

       // ----------------------------------------------------

       endif;

      /end-free
     p K3S_Calc_Usage...
     p                 E

       //****************************************************************
       //  Subprocedure: K3S_Manual
       //  Desc: Calculate manual min/max values
       //  (From K3S_C130 and K3S_C131)
       //**************************************************************
     p K3S_Calc_Manual...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  altsour                       1  0 const
     d  maminid                       3  0
     d  mamaxid                       3  0
     d  maminiu                       7  0
     d  mamaxiu                       7  0
     d  qtybaln                       7  0
     d  chkopnt                       1  0
     d  sochk                         5  0
     d  seasonl                      10    const
     d  forcast                       9  2 const
     d  forcper                       3  0 const
     d  forcint                       3  0 const
     d  longtrm                       3  2 const
     d  minday                        9  3
     d  maxday                        9  3
     d  minunit                       7  0
     d  maxunit                       7  0
     d  mnwork                        7  0
     d  mxwork                        7  0
     d  str_day                       1  0 const
     d  str_unt                       1  0 const
     d*
     d  comp                          1    const
     d  days                         11  3
     d  dowmap                        1  0
     d  dowper1                       5  2
     d  dowper2                       5  2
     d  dowper3                       5  2
     d  dowper4                       5  2
     d  dowper5                       5  2
     d  dowper6                       5  2
     d  dowper7                       5  2
     d  sstper1                       5  2
     d  sstper2                       5  2
     d  sstper3                       5  2
     d  sstper4                       5  2
     d  sstper5                       5  2
     d  sstper6                       5  2
     d  sstper7                       5  2
     d  diff12                        3  0 const
     d  diff13                        3  0 const
     d  diff52                        3  0 const
     d  use                          15  3
     d  DS_flag                       1  0
     d  DayNbr                        1  0
     d  ix                            5s 2 dim(52)
     d  Ds_Days                       2  0
     d  Ds_wdis                       5  2 dim(12)
     d  Ds_dlyu                      15  4 dim(12)
     d  Ds_spct                       5  2 dim(12)
     d  Ds_susg                      15  4 dim(12)
     d  Ds_accu                      15  4 dim(12)
     d  Ds_prno                       3  0 dim(12)
     d  Ds_fctr                       5  2 dim(12)
     d  Ds_warn                       1  0

      * ------------------------ ---------------------------local variables
     d*use             s             15  3
     d*days            s             11  3
      * ------------------------ ---------------------------parameters
      /free

       exsr calc_units;
       exsr calc_days;
       // ------------------------ logic for manual minimum and maximum values

       begsr calc_units;
       //  units begin -------------------------------------------------------

       //    manual minimum units exist, OR manual maximum units exist
       if maminiu > 0 OR mamaxiu > 0;

       //         order point check for manual minimum
          if altsour = 0 and
             chkopnt = 0 and
             qtybaln < maminiu;

             chkopnt = 1;
             sochk += 1;
          endif;

       //    use straight units
          if str_unt = 1;
             if maminiu = 0;
                maminiu = mamaxiu;
             endif;
             if mamaxiu = 0;
                mamaxiu = maminiu;
             endif;
             minunit = maminiu;
             maxunit = mamaxiu;
           endif;

          if minunit <= maminiu;
             mnwork = 0;
             minunit = maminiu;
             exsr bypass;
          else;
             if mamaxiu = 0;
                leavesr;
             else;
                if minunit > mamaxiu;
                   mnwork = 0;
                   minunit = mamaxiu;
                endif;
                exsr bypass;
             endif;
          endif;
       endif;

       endsr;

       begsr bypass;

       if mamaxiu = 0;
          leavesr;
       else;
          if maxunit > 0 and
             maxunit < mamaxiu;

             leavesr;
          else;
             maxunit = mamaxiu;
             mxwork  = 0;
          endif;
       endif;

       endsr;

       //  units end ---------------------------------------------------------


       begsr calc_days;
       //  days begin --------------------------------------------------------
       //    manual minimum days exist, OR manual maximum days exist
       if maminid > 0 OR mamaxid > 0;

       //    use straight days
          if str_day = 1;
             if maminid = 0;
                maminid = mamaxid;
             endif;
             if mamaxid = 0;
                mamaxid = maminid;
             endif;
             mnwork = maminid;
             mxwork = mamaxid;
          endif;

          if str_day = 0;
             if mnwork >=  maminid;
                exsr bypday;
             else;
       //
       //     calculate manual minimum units
                clear use;
                days = maminid;
                callp K3S_Calc_Usage(comp:
                                     seasonl:
                                     days:
                                     forcast:
                                     forcper:
                                     forcint:
                                     longtrm:
                                     dowmap:
                                     dowper1:
                                     dowper2:
                                     dowper3:
                                     dowper4:
                                     dowper5:
                                     dowper6:
                                     dowper7:
                                     sstper1:
                                     sstper2:
                                     sstper3:
                                     sstper4:
                                     sstper5:
                                     sstper6:
                                     sstper7:
                                     diff12:
                                     diff13:
                                     diff52:
                                     use:
                                     Ds_flag:
                                     DayNbr:
                                     ix:
                                     Ds_days:
                                     Ds_wdis:
                                     Ds_dlyu:
                                     Ds_spct:
                                     Ds_susg:
                                     Ds_accu:
                                     Ds_prno:
                                     Ds_fctr:
                                     Ds_warn);
       //         exsr $_usage;
       //
                mnwork = maminid;
                eval(h) minunit = use;

                if minunit = 0 and
                   use > 0;

                   minunit = 1;
                endif;
                exsr bypday;
             endif;
          else;
             clear use;
             days = maminid;
             callp K3S_Calc_Usage(comp:
                                  seasonl:
                                  days:
                                  forcast:
                                  forcper:
                                  forcint:
                                  longtrm:
                                  dowmap:
                                  dowper1:
                                  dowper2:
                                  dowper3:
                                  dowper4:
                                  dowper5:
                                  dowper6:
                                  dowper7:
                                  sstper1:
                                  sstper2:
                                  sstper3:
                                  sstper4:
                                  sstper5:
                                  sstper6:
                                  sstper7:
                                  diff12:
                                  diff13:
                                  diff52:
                                  use:
                                  Ds_flag:
                                  DayNbr:
                                  ix:
                                  Ds_days:
                                  Ds_wdis:
                                  Ds_dlyu:
                                  Ds_spct:
                                  Ds_susg:
                                  Ds_accu:
                                  Ds_prno:
                                  Ds_fctr:
                                  Ds_warn);
       //    exsr $_usage;
             mnwork = maminid;
             eval(h) minunit = use;

             if minunit = 0 and
                use > 0;

                minunit = 1;
             endif;
             exsr bypday;
          endif;

          if maminid = 0 and
             mamaxid > 0 and
             maxunit = minunit;

             mnwork = mxwork;
          endif;
          if mxwork < mnwork;
             mxwork = mnwork;
             maxunit = minunit;
          endif;
          if altsour = 0 and
             chkopnt = 0 and
             qtybaln < minunit;

             chkopnt = 1;
             sochk += 1;

          endif;
       endif;
       endsr;

       begsr bypday;

       if mamaxid = 0;
          leavesr;
       endif;

       if str_day = 0;
          if mxwork <= mamaxid;
             leavesr;
          else;
             mxwork = mamaxid;
             clear use;
             days = mamaxid;
             callp K3S_Calc_Usage(comp:
                                  seasonl:
                                  days:
                                  forcast:
                                  forcper:
                                  forcint:
                                  longtrm:
                                  dowmap:
                                  dowper1:
                                  dowper2:
                                  dowper3:
                                  dowper4:
                                  dowper5:
                                  dowper6:
                                  dowper7:
                                  sstper1:
                                  sstper2:
                                  sstper3:
                                  sstper4:
                                  sstper5:
                                  sstper6:
                                  sstper7:
                                  diff12:
                                  diff13:
                                  diff52:
                                  use:
                                  Ds_flag:
                                  DayNbr:
                                  ix:
                                  Ds_days:
                                  Ds_wdis:
                                  Ds_dlyu:
                                  Ds_spct:
                                  Ds_susg:
                                  Ds_accu:
                                  Ds_prno:
                                  Ds_fctr:
                                  Ds_warn);

             eval(h) maxunit = use;

             if (maxunit = 0) and
                (use > 0);

                maxunit = 1;
             endif;

             if maxunit < minunit;
                minunit = maxunit;
                mnwork = mxwork;
             endif;
          endif;
       else;
          mxwork = mamaxid;
          clear use;
          days = mamaxid;
          callp K3S_Calc_Usage(comp:
                               seasonl:
                               days:
                               forcast:
                               forcper:
                               forcint:
                               longtrm:
                               dowmap:
                               dowper1:
                               dowper2:
                               dowper3:
                               dowper4:
                               dowper5:
                               dowper6:
                               dowper7:
                               sstper1:
                               sstper2:
                               sstper3:
                               sstper4:
                               sstper5:
                               sstper6:
                               sstper7:
                               diff12:
                               diff13:
                               diff52:
                               use:
                               Ds_flag:
                               DayNbr:
                               ix:
                               Ds_days:
                               Ds_wdis:
                               Ds_dlyu:
                               Ds_spct:
                               Ds_susg:
                               Ds_accu:
                               Ds_prno:
                               Ds_fctr:
                               Ds_warn);

          eval(h) maxunit = use;

          if (maxunit = 0) and
             (use > 0);

             maxunit = 1;
          endif;

          if maxunit < minunit;
             minunit = maxunit;
             mnwork = mxwork;
          endif;

       endif;
       endsr;
       //  days end ----------------------------------------------------------
      /end-free
     p K3S_Calc_Manual...
     p                 E

       //****************************************************************
       //  Subprocedure: K3S_Calc_Manual_R
       //  Desc: Calculate manual min/max values
       //  (From K3S_C130 and K3S_C131_R)
       //
       //   10/12/2015 - reversed, days 1st, then units 2nd.
       //                Variety Wholesale for ReStore providing
       //                Manual Min Units - presentation stock
       //                Manual Max Days  - size of stores 21, 28, 35 days
       //////////////////////////////////////////////////////////////////
     p K3S_Calc_Manual_R...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  altsour                       1  0 const
     d  maminid                       3  0
     d  mamaxid                       3  0
     d  maminiu                       7  0
     d  mamaxiu                       7  0
     d  qtybaln                       7  0
     d  chkopnt                       1  0
     d  sochk                         5  0
     d  seasonl                      10    const
     d  forcast                       9  2 const
     d  forcper                       3  0 const
     d  forcint                       3  0 const
     d  longtrm                       3  2 const
     d  minday                        9  3
     d  maxday                        9  3
     d  minunit                       7  0
     d  maxunit                       7  0
     d  mnwork                        7  0
     d  mxwork                        7  0
     d  str_day                       1  0 const
     d  str_unt                       1  0 const
     d*
     d  comp                          1    const
     d  days                         11  3
     d  dowmap                        1  0
     d  dowper1                       5  2
     d  dowper2                       5  2
     d  dowper3                       5  2
     d  dowper4                       5  2
     d  dowper5                       5  2
     d  dowper6                       5  2
     d  dowper7                       5  2
     d  sstper1                       5  2
     d  sstper2                       5  2
     d  sstper3                       5  2
     d  sstper4                       5  2
     d  sstper5                       5  2
     d  sstper6                       5  2
     d  sstper7                       5  2
     d  diff12                        3  0 const
     d  diff13                        3  0 const
     d  diff52                        3  0 const
     d  use                          15  3
     d  DS_flag                       1  0
     d  DayNbr                        1  0
     d  ix                            5s 2 dim(52)
     d  Ds_Days                       2  0
     d  Ds_wdis                       5  2 dim(12)
     d  Ds_dlyu                      15  4 dim(12)
     d  Ds_spct                       5  2 dim(12)
     d  Ds_susg                      15  4 dim(12)
     d  Ds_accu                      15  4 dim(12)
     d  Ds_prno                       3  0 dim(12)
     d  Ds_fctr                       5  2 dim(12)
     d  Ds_warn                       1  0

      * ------------------------ ---------------------------local variables
     d*use             s             15  3
     d*days            s             11  3
      * ------------------------ ---------------------------parameters
      /free
       // Days first then units
       exsr calc_days;
       exsr calc_units;
       // --------------------- Calculate manual minimum and maximum values

       begsr calc_days;
       //  days begin --------------------------------------------------------
       //    manual minimum days exist, OR manual maximum days exist
       if maminid > 0 OR mamaxid > 0;

       //    use straight days
          if str_day = 1;
             if maminid = 0;
                maminid = mamaxid;
             endif;
             if mamaxid = 0;
                mamaxid = maminid;
             endif;
             mnwork = maminid;
             mxwork = mamaxid;
          endif;

          if str_day = 0;
             if mnwork >=  maminid;
                exsr bypday;
             else;
       //
       //     calculate manual minimum units
                clear use;
                days = maminid;
                callp K3S_Calc_Usage(comp:
                                     seasonl:
                                     days:
                                     forcast:
                                     forcper:
                                     forcint:
                                     longtrm:
                                     dowmap:
                                     dowper1:
                                     dowper2:
                                     dowper3:
                                     dowper4:
                                     dowper5:
                                     dowper6:
                                     dowper7:
                                     sstper1:
                                     sstper2:
                                     sstper3:
                                     sstper4:
                                     sstper5:
                                     sstper6:
                                     sstper7:
                                     diff12:
                                     diff13:
                                     diff52:
                                     use:
                                     Ds_flag:
                                     DayNbr:
                                     ix:
                                     Ds_days:
                                     Ds_wdis:
                                     Ds_dlyu:
                                     Ds_spct:
                                     Ds_susg:
                                     Ds_accu:
                                     Ds_prno:
                                     Ds_fctr:
                                     Ds_warn);
       //         exsr $_usage;
       //
                mnwork = maminid;
                eval(h) minunit = use;

                if minunit = 0 and
                   use > 0;

                   minunit = 1;
                endif;
                exsr bypday;
             endif;
          else;
             clear use;
             days = maminid;
             callp K3S_Calc_Usage(comp:
                                  seasonl:
                                  days:
                                  forcast:
                                  forcper:
                                  forcint:
                                  longtrm:
                                  dowmap:
                                  dowper1:
                                  dowper2:
                                  dowper3:
                                  dowper4:
                                  dowper5:
                                  dowper6:
                                  dowper7:
                                  sstper1:
                                  sstper2:
                                  sstper3:
                                  sstper4:
                                  sstper5:
                                  sstper6:
                                  sstper7:
                                  diff12:
                                  diff13:
                                  diff52:
                                  use:
                                  Ds_flag:
                                  DayNbr:
                                  ix:
                                  Ds_days:
                                  Ds_wdis:
                                  Ds_dlyu:
                                  Ds_spct:
                                  Ds_susg:
                                  Ds_accu:
                                  Ds_prno:
                                  Ds_fctr:
                                  Ds_warn);
       //    exsr $_usage;
             mnwork = maminid;
             eval(h) minunit = use;

             if minunit = 0 and
                use > 0;

                minunit = 1;
             endif;
             exsr bypday;
          endif;

          if maminid = 0 and
             mamaxid > 0 and
             maxunit = minunit;

             mnwork = mxwork;
          endif;
          if mxwork < mnwork;
             mxwork = mnwork;
             maxunit = minunit;
          endif;
          if altsour = 0 and
             chkopnt = 0 and
             qtybaln < minunit;

             chkopnt = 1;
             sochk += 1;

          endif;
       endif;
       endsr;

       begsr bypday;

       if mamaxid = 0;
          leavesr;
       endif;

       if str_day = 0;
          if mxwork <= mamaxid;
             leavesr;
          else;
             mxwork = mamaxid;
             clear use;
             days = mamaxid;
             callp K3S_Calc_Usage(comp:
                                  seasonl:
                                  days:
                                  forcast:
                                  forcper:
                                  forcint:
                                  longtrm:
                                  dowmap:
                                  dowper1:
                                  dowper2:
                                  dowper3:
                                  dowper4:
                                  dowper5:
                                  dowper6:
                                  dowper7:
                                  sstper1:
                                  sstper2:
                                  sstper3:
                                  sstper4:
                                  sstper5:
                                  sstper6:
                                  sstper7:
                                  diff12:
                                  diff13:
                                  diff52:
                                  use:
                                  Ds_flag:
                                  DayNbr:
                                  ix:
                                  Ds_days:
                                  Ds_wdis:
                                  Ds_dlyu:
                                  Ds_spct:
                                  Ds_susg:
                                  Ds_accu:
                                  Ds_prno:
                                  Ds_fctr:
                                  Ds_warn);

             eval(h) maxunit = use;

             if (maxunit = 0) and
                (use > 0);

                maxunit = 1;
             endif;

             if maxunit < minunit;
                minunit = maxunit;
                mnwork = mxwork;
             endif;
          endif;
       else;
          mxwork = mamaxid;
          clear use;
          days = mamaxid;
          callp K3S_Calc_Usage(comp:
                               seasonl:
                               days:
                               forcast:
                               forcper:
                               forcint:
                               longtrm:
                               dowmap:
                               dowper1:
                               dowper2:
                               dowper3:
                               dowper4:
                               dowper5:
                               dowper6:
                               dowper7:
                               sstper1:
                               sstper2:
                               sstper3:
                               sstper4:
                               sstper5:
                               sstper6:
                               sstper7:
                               diff12:
                               diff13:
                               diff52:
                               use:
                               Ds_flag:
                               DayNbr:
                               ix:
                               Ds_days:
                               Ds_wdis:
                               Ds_dlyu:
                               Ds_spct:
                               Ds_susg:
                               Ds_accu:
                               Ds_prno:
                               Ds_fctr:
                               Ds_warn);

          eval(h) maxunit = use;

          if (maxunit = 0) and
             (use > 0);

             maxunit = 1;
          endif;

          if maxunit < minunit;
             minunit = maxunit;
             mnwork = mxwork;
          endif;

       endif;
       endsr;
       //  days end ----------------------------------------------------------

       begsr calc_units;
       //  units begin -------------------------------------------------------

       //    manual minimum units exist, OR manual maximum units exist
       if maminiu > 0 OR mamaxiu > 0;

       //         order point check for manual minimum
          if altsour = 0 and
             chkopnt = 0 and
             qtybaln < maminiu;

             chkopnt = 1;
             sochk += 1;
          endif;

       //    use straight units
          if str_unt = 1;
             if maminiu = 0;
                maminiu = mamaxiu;
             endif;
             if mamaxiu = 0;
                mamaxiu = maminiu;
             endif;
             minunit = maminiu;
             maxunit = mamaxiu;
           endif;

          if minunit <= maminiu;
             mnwork = 0;
             minunit = maminiu;
             exsr bypass;
          else;
             if mamaxiu = 0;
                leavesr;
             else;
                if minunit > mamaxiu;
                   mnwork = 0;
                   minunit = mamaxiu;
                endif;
                exsr bypass;
             endif;
          endif;
       endif;

       endsr;

       begsr bypass;

       if mamaxiu = 0;
          leavesr;
       else;
          if maxunit > 0 and
             maxunit < mamaxiu;

             leavesr;
          else;
             maxunit = mamaxiu;
             mxwork  = 0;
          endif;
       endif;

       endsr;

       //  units end ---------------------------------------------------------
      /end-free
     p K3S_Calc_Manual_R...
     p                 E
