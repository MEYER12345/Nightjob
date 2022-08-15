      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

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
      **
      **   Name: K3S_9998
      **   Type: ILE RPG Program
      **   Desc: Get K_TABLCOD records for NITECL Parms. type NJP
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 09/19/2014.
      *   Remarks. Altered program to use embedded SQL statements to
      *            retrieve parameter values from K_TABLCOD records
      *            with type 'NJP'.
      *****************************************************************

      * -------------------------------------- common D specs
     d*copy k3s_c270

     d validsupl       s             21a
     d taflag1         s              1s 0
     d tacodeds3       s            100
     d
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------  procedure interface
     d K3S_9998        PI
     d  comp                          1
     d  alt_system                    1
     d  per_filter                    1
     d  transfer                      1
     d  overstock                     1
     d  log_report                    1
     d  log_type_5                    1
     d  prod_link                     1
     d  valid_proc                    1
     d  valid_supl                   10
     d  valid_sub                    10
     d  first_time                    1
     d  auto_po                       1
     d  rmv_histry                    1
      * -----------------------------------------------------------------------
      /free
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'ALT_SYSTEM          '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          alt_system = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'PER_FILTER          '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          per_filter = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'TRANSFER            '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          transfer = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'OVERSTOCK           '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          overstock = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'LOG_REPORT          '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          log_report = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'LOG_TYPE_5          '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          log_type_5 = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'PROD_LINK           '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          prod_link = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1, ta_codeds3
           into :taflag1, :tacodeds3
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'VALID_PROC          '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          valid_proc = %editc(taflag1:'X');
          validsupl = tacodeds3;
          valid_supl = validsupl;
          evalr valid_sub = validsupl;
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'FIRST_TIME          '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          first_time = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'AUTO_PO             '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          auto_po = %editc(taflag1:'X');
       endif;
       //
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
         where ta_comp = :comp and ta_codetyp = 'NJP' and
                         ta_codeval = 'RMV_HISTRY          '
         fetch first row only;
       //
       If SQLSTT = SQLStateOk;                   //Successfully found
          rmv_histry = %editc(taflag1:'X');
       endif;
       //
       *inlr = *on;
      /end-free
