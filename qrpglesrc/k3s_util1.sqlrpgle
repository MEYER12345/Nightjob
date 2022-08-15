      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h OPTION(*NODEBUGIO)

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
      **   Name: K3S_UTIL1
      **   Type: ILE RPG Program
      **   Desc: Delete records matching passed location parameter.
      **
      *****************************************************************
      **
      *****************************************************************
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * ------------------------------------------------------parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_UTIL1       PI
     d  locn                          5a
      * ---------------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                  datfmt = *iso;
       exec sql
        Delete
          From k_suplier
          Where sp_locn = :locn;
       //
        if SQLState = SQLStateOk;
       //
           exec sql
            Delete
              From k_product
              Where pr_locn = :locn;
       //
            if SQLState = SQLStateOk;
       //
               exec sql
                Delete
                  From k_prodhis
                  Where ph_locn = :locn;
            endif;
        endif;
       //
       *inlr = '1';
      /end-free
