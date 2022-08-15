      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 6.01 2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO) datfmt(*iso)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2013 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: PFDESCRB
      **   Type: ILE RPG Program
      **   Desc: Web - Build Prototype Headers...
      **   Auth: Carlos Palacios
      **
      ******************************************************************
      **                                                               *
      **  This program is used to create headers and Signatures for    *
      **  Service Programs based on K3S Physical Files                 *
      **                                                               *
      **                                                               *
      **   Change ID  Change Date Change Description                   *
      **   ---------  ----------  -------------------------------------*
      **              07-12-2016  Initially written.                   *
      ******************************************************************
     fqsqlgen   O    e             disk    rename(qsqlgen:RK_protos)
     f                                     extfile(file01)
     f                                     extmbr(memberid)
     f                                     usropn
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d dspfdescr     e ds                  ExtName(pfdescr)
      * -------------------------------------------------------
     d command         s            512
     d cmdlength       s             15  5 inz(512)
     d quote           s              1    dim(1) ctdata perrcd(1)
     d arcmd           s              1    dim(512)
     d memberid        s             10    inz
     d file01          s             21    inz('DMEYER/QSQLGEN')
     d wkcdat          s              6  0 inz
     d wkx             s              6  2 inz
     d wky             s              2  0 inz
     d wki             s              3  0 inz
     d wkv             s              3  0 inz
     d wktwo           s              2    inz
     d wktwod          s              2    inz
     d wkkey01         s              7    inz
     d date            s               d   datfmt(*YMD)
     d wkclear         c                   const('clear();')
     d wkdelete        c                   const('Delete')
     d getrcd          c                   const('record...')
     d update          c                   const('update...')
     d insert          c                   const('insert...')
     d delete          c                   const('delete...')
     d setpgm          c                   const('STRPGMEXP  PGMLVL(*CURRENT) s-
     d                                     ignature(')
     d srvpgm          c                   const('Access SRVPGM')
     d export          c                   const('  EXPORT     SYMBOL(')
     d dashes          s             70    inz
      * -------------------------------------------------- Arrays and the likes...
     d dsInsert        ds
     d   ArrIns                      55    Dim(99)                                   Array 4 Insert
     d     Inf01                     10    overlay(Arrins)                           Insert Field 01
     d     Inc01                      1    overlay(Arrins:*next)                     Insert Comma 01
     d     Inf02                     10    overlay(Arrins:*next)                     Insert Field 02
     d     Inc02                      1    overlay(Arrins:*next)                     Insert Comma 02
     d     Inf03                     10    overlay(Arrins:*next)                     Insert Field 03
     d     Inc03                      1    overlay(Arrins:*next)                     Insert Comma 03
     d     Inf04                     10    overlay(Arrins:*next)                     Insert Field 04
     d     Inc04                      1    overlay(Arrins:*next)                     Insert Comma 04
     d     Inf05                     10    overlay(Arrins:*next)                     Insert Field 05
     d     Inc05                      1    overlay(Arrins:*next)                     Insert Comma 05
     d                                                                               Dimension 4 Ins
     d dsValues        ds
     d   ArrVal                      60    Dim(99)                                   Array 4 Values
     d     Vaf01                     11    overlay(Arrval)                           Value  Field 01
     d     Vac01                      1    overlay(Arrval:*next)                     Value  Comma 01
     d     Vaf02                     11    overlay(Arrval:*next)                     Value  Field 02
     d     Vac02                      1    overlay(Arrval:*next)                     Value  Comma 02
     d     Vaf03                     11    overlay(Arrval:*next)                     Value  Field 03
     d     Vac03                      1    overlay(Arrval:*next)                     Value  Comma 03
     d     Vaf04                     11    overlay(Arrval:*next)                     Value  Field 04
     d     Vac04                      1    overlay(Arrval:*next)                     Value  Comma 04
     d     Vaf05                     11    overlay(Arrval:*next)                     Value  Field 05
     d     Vac05                      1    overlay(Arrval:*next)                     Value  Comma 05
     d                                                                               Dimension 4 Ins
     d dsUpdate        ds
     d   ArrUpd                     125    Dim(99)                                   Array 4 UPdate
     d     Upf01                     24    overlay(Arrupd)                           updert Field 01
     d     Upc01                      1    overlay(Arrupd:*next)                     updert Comma 01
     d     Upf02                     24    overlay(Arrupd:*next)                     updert Field 02
     d     Upc02                      1    overlay(Arrupd:*next)                     updert Comma 02
     d     Upf03                     24    overlay(Arrupd:*next)                     updert Field 03
     d     Upc03                      1    overlay(Arrupd:*next)                     updert Comma 03
     d     Upf04                     24    overlay(Arrupd:*next)                     updert Field 04
     d     Upc04                      1    overlay(Arrupd:*next)                     updert Comma 04
     d     Upf05                     24    overlay(Arrupd:*next)                     updert Field 05
     d     Upc05                      1    overlay(Arrupd:*next)                     updert Comma 05
     d
      * -------------------------------------------------- Prototype Parameters received
     d PFDESCRB        PR                  extpgm('PFDESCRB')
     d  Process                       3                                              Get or Set
     d  Fileid                       10                                              File ID
     d  Filehd                        9                                              File Header
     d  Protog                       11                                              File + Get
     d  Protos                       11                                              File + Set
     d  Keyflds                       2  0                                           Key fields
     d  Key01                        10                                              Access Key 01
     d  Key02                        10                                              Access Key 02
     d  Key03                        10                                              Access Key 03
     d  Key04                        10                                              Access Key 04
     d  Key05                        10                                              Access Key 05
     d  Key06                        10                                              Access Key 06
     d  Key07                        10                                              Access Key 07
     d  Key08                        10                                              Access Key 08
     d  Key09                        10                                              Access Key 09
     d  Key10                        10                                              Access Key 10
     d  Len01                        05                                              Access Len 01
     d  Len02                        05                                              Access Len 02
     d  Len03                        05                                              Access Len 03
     d  Len04                        05                                              Access Len 04
     d  Len05                        05                                              Access Len 05
     d  Len06                        05                                              Access Len 06
     d  Len07                        05                                              Access Len 07
     d  Len08                        05                                              Access Len 08
     d  Len09                        05                                              Access Len 09
     d  Len10                        05                                              Access Len 10
     d  Servp                        10                                             Service Program
     d  Nam01                        10                                              Access Nam 01
     d  Nam02                        10                                              Access Nam 02
     d  Nam03                        10                                              Access Nam 03
     d  Nam04                        10                                              Access Nam 04
     d  Nam05                        10                                              Access Nam 05
     d  Nam06                        10                                              Access Nam 06
     d  Nam07                        10                                              Access Nam 07
     d  Nam08                        10                                              Access Nam 08
     d  Nam09                        10                                              Access Nam 09
     d  Nam10                        10                                              Access Nam 10
      * -------------------------------------------------- Procedure interface
     d PFDESCRB        PI
     d  Process                       3                                              Get or Set
     d  Fileid                       10                                              File ID
     d  Filehd                        9                                              File Header
     d  Protog                       11                                              File + Get
     d  Protos                       11                                              File + Set
     d  Keyflds                       2  0                                           Key fields
     d  Key01                        10                                              Access Key 01
     d  Key02                        10                                              Access Key 02
     d  Key03                        10                                              Access Key 03
     d  Key04                        10                                              Access Key 04
     d  Key05                        10                                              Access Key 05
     d  Key06                        10                                              Access Key 06
     d  Key07                        10                                              Access Key 07
     d  Key08                        10                                              Access Key 08
     d  Key09                        10                                              Access Key 09
     d  Key10                        10                                              Access Key 10
     d  Len01                        05                                              Access Len 01
     d  Len02                        05                                              Access Len 02
     d  Len03                        05                                              Access Len 03
     d  Len04                        05                                              Access Len 04
     d  Len05                        05                                              Access Len 05
     d  Len06                        05                                              Access Len 06
     d  Len07                        05                                              Access Len 07
     d  Len08                        05                                              Access Len 08
     d  Len09                        05                                              Access Len 09
     d  Len10                        05                                              Access Len 10
     d  Servp                        10                                             Service Program
     d  Nam01                        10                                              Access Nam 01
     d  Nam02                        10                                              Access Nam 02
     d  Nam03                        10                                              Access Nam 03
     d  Nam04                        10                                              Access Nam 04
     d  Nam05                        10                                              Access Nam 05
     d  Nam06                        10                                              Access Nam 06
     d  Nam07                        10                                              Access Nam 07
     d  Nam08                        10                                              Access Nam 08
     d  Nam09                        10                                              Access Nam 09
     d  Nam10                        10                                              Access Nam 10
      **
      /free

       //-------------------------------------------------------- Begin
        exec sql

           set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       //------------------------------------------------- Set proper variables...

         date = %date();
         wkx = 2;
         dashes = (*all'-');
         select;
          when Process = 'SET';
            wkx = 250;
          when Process = 'SYG';
            wkx = 1;
          when Process = 'SYS';
            wkx = 250;
          when Process = 'SQG';
            wkx = 1;
          when Process = 'SQS';
            wkx = 250;
          when Process = 'DEL';
            wkx = 1000;
          when Process = 'UPD';
            wkx = 2000;
          when Process = 'INS';
            wkx = 3000;
         endsl;

         memberid = filehd;
         if Process = 'SQG' or Process = 'SQS' or
            Process = 'DEL' or Process = 'INS' or
            Process = 'UPD';
            memberid = servp;
         endif;

         if Process = 'INS' or Process = 'UPD';
            clear ArrIns;
            clear ArrVal;
            clear Arrupd;
         endif;

       //open K_PROTOS;
         open QSQLGEN;
       //------------------------------------------------- Declare Cursor...

         exec sql
          Declare pfcursor Cursor
           for
            select *
             from pfdescr
              where whfile = :Fileid
               with NC;

       //------------------------------------------------- Close   Cursor...

         exec sql
          close pfcursor;

       //------------------------------------------------- Open    Cursor...

         exec sql
          open pfcursor;

       //------------------------------------------------- Fetch   Cursor...

       //wkcdat = %dec(%char(date(6:0)*YMD));
       Dow SQLState = SQLStateOK;
         exec sql
          fetch next
           from pfcursor
            into :dspfdescr;

         if SQLState = RowNotFound;
            leave;
         endif;

         select;
          when Process = 'GET';
            exsr $_bld_Gets;
          when Process = 'SET';
            exsr $_bld_Sets;
          when Process = 'SYG';
            exsr $_bld_Symg;
          when Process = 'SYS';
            exsr $_bld_Syms;
          when Process = 'SQG';
            exsr $_bld_Sqlg;
          when Process = 'SQS';
            exsr $_bld_Sqls;
          when Process = 'DEL';
            exsr $_bld_Dels;
            leave;
          when Process = 'UPD';
            exsr $_bld_Upds;
          when Process = 'INS';
            exsr $_bld_Inss;
         endsl;

       enddo;

       //------------------------------------------------- Close   Cursor...
         exec sql
          close pfcursor;

       select;
        when Process = 'SET';
          exsr $_bld_Dbfh;
        when Process = 'SYS';
          exsr $_bld_Dbfs;
        when Process = 'INS';
          exsr $_Wrt_Inss;
        when Process = 'UPD';
          exsr $_Wrt_Upds;
       endsl;

       *inlr = *On;
       close QSQLGEN;
       return;


        //------------------------------------------------------------------
        //   Build Get Records...
        //------------------------------------------------------------------
         begsr $_bld_Gets;

           if whfldd = 0;
              wktwo = %subst(%editc(wHFLDB : 'X'):4:2);
           else;
              wktwo = %subst(%editc(wHFLDD : 'X'):1:2);
           endif;

           Select;
             when wkx = 2;
                %subst(srcdta:6:60) = 'd ' + protog  + getrcd;
                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;
                %subst(srcdta:6:2) = 'd ';
                %subst(srcdta:24:2)= 'pr';
                %subst(srcdta:40:1)= 'n';
                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                wky = 1;
                dow wky <= keyflds;
                  Select;
                     When wky = 1;
                        %subst(srcdta:6:30)= 'd ' + key01;
                        %subst(srcdta:38:5)= len01;
                     When wky = 2;
                        %subst(srcdta:6:30)= 'd ' + key02;
                        %subst(srcdta:38:5)= len02;
                     When wky = 3;
                        %subst(srcdta:6:30)= 'd ' + key03;
                        %subst(srcdta:38:5)= len03;
                     When wky = 4;
                        %subst(srcdta:6:30)= 'd ' + key04;
                        %subst(srcdta:38:5)= len04;
                     When wky = 5;
                        %subst(srcdta:6:30)= 'd ' + key05;
                        %subst(srcdta:38:5)= len05;
                     When wky = 6;
                        %subst(srcdta:6:30)= 'd ' + key06;
                        %subst(srcdta:38:5)= len06;
                     When wky = 7;
                        %subst(srcdta:6:30)= 'd ' + key07;
                        %subst(srcdta:38:5)= len07;
                     When wky = 8;
                        %subst(srcdta:6:30)= 'd ' + key08;
                        %subst(srcdta:38:5)= len08;
                     When wky = 9;
                        %subst(srcdta:6:30)= 'd ' + key09;
                        %subst(srcdta:38:5)= len09;
                     When wky =10;
                        %subst(srcdta:6:30)= 'd ' + key10;
                        %subst(srcdta:38:5)= len10;
                  endsl;

                  srcseq = wkx;

                  write rk_protos;
                  srcdat = wkcdat;
                  clear srcdta;
                  wkx  += 1;
                  wky += 1;

                enddo;

                %subst(srcdta:6:30)= 'd ' + protog + %trimr(whfldi) +
                '...';
                srcseq = wkx;
                srcdat = wkcdat;

                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:2) = 'd ';
                %subst(srcdta:24:2)= 'pr';
   63           %subst(srcdta:38:2)= wktwo;
   63           %subst(srcdta:40:1)= whfldt;

                if whfldt = 'P';
                %subst(srcdta:40:1)= ' ';
                   wktwod = %editc(whfldp: 'X');
                   %subst(srcdta:42:1)= %subst(wktwod:2:1);
                endif;

                if %subst(srcdta:38:1)= '0';
                   %subst(srcdta:38:1)= ' ';
                endif;

                if whfldt = 'L';
                   %subst(srcdta:38:3)= '  D';
                endif;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

             other;

                %subst(srcdta:6:30)= 'd ' + protog + %trimr(whfldi) +
                '...';
                srcseq = wkx;
                srcdat = wkcdat;

                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:2) = 'd ';
                %subst(srcdta:24:2)= 'pr';
                %subst(srcdta:38:2)= wktwo;
                %subst(srcdta:40:1)= whfldt;

                if whfldt = 'P';
                   %subst(srcdta:40:1)= ' ';
                   wktwod = %editc(whfldp: 'X');
                   %subst(srcdta:42:1)= %subst(wktwod:2:1);
                endif;

                if %subst(srcdta:38:1)= '0';
                   %subst(srcdta:38:1)= ' ';
                endif;


                if whfldt = 'L';
                   %subst(srcdta:38:3)= '  d';
                endif;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

           endsl;

         endsr;

        //------------------------------------------------------------------
        //   Build Set Records...
        //------------------------------------------------------------------
         begsr $_bld_Sets;

                if whfldd = 0;
                   wktwo = %subst(%editc(wHFLDB : 'X'):4:2);
                else;
                   wktwo = %subst(%editc(wHFLDD : 'X'):1:2);
                endif;

                %subst(srcdta:6:30)= 'd ' + protos + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:2) = 'd ';
                %subst(srcdta:24:2)= 'pr';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:2) = 'd ';
                %subst(srcdta:8:30)= 'Pm' + %subst(whfldi:4:7);
                %subst(srcdta:38:2)= wktwo;
                %subst(srcdta:40:1)= whfldt;
                %subst(srcdta:44:5)= 'const';

                if whfldt = 'P';
                   %subst(srcdta:40:1)= ' ';
                   wktwod = %editc(whfldp: 'X');
                   %subst(srcdta:42:1)= %subst(wktwod:2:1);
                endif;

                if %subst(srcdta:38:1)= '0';
                   %subst(srcdta:38:1)= ' ';
                endif;


                if whfldt = 'L';
                   %subst(srcdta:38:3)= '  d';
                endif;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;


         endsr;

        //------------------------------------------------------------------
        //   Build Database Function Headers...
        //------------------------------------------------------------------
         begsr $_bld_Dbfh;

                %subst(srcdta:6:70)= ' *' + dashes;


                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Database Functions ---------------*';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:60) = 'd ' + %subst(protog:1:8) +
                'clear   pr';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:60) = 'd ' + %subst(protog:1:8) +
                'update  pr';
                %subst(srcdta:40:1)= 'n';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                wky = 1;
                dow wky <= keyflds;
                  Select;
                     When wky = 1;
                        %subst(srcdta:6:30)= 'd ' + key01;
                        %subst(srcdta:38:5)= len01;
                     When wky = 2;
                        %subst(srcdta:6:30)= 'd ' + key02;
                        %subst(srcdta:38:5)= len02;
                     When wky = 3;
                        %subst(srcdta:6:30)= 'd ' + key03;
                        %subst(srcdta:38:5)= len03;
                     When wky = 4;
                        %subst(srcdta:6:30)= 'd ' + key04;
                        %subst(srcdta:38:5)= len04;
                     When wky = 5;
                        %subst(srcdta:6:30)= 'd ' + key05;
                        %subst(srcdta:38:5)= len05;
                     When wky = 6;
                        %subst(srcdta:6:30)= 'd ' + key06;
                        %subst(srcdta:38:5)= len06;
                     When wky = 7;
                        %subst(srcdta:6:30)= 'd ' + key07;
                        %subst(srcdta:38:5)= len07;
                     When wky = 8;
                        %subst(srcdta:6:30)= 'd ' + key08;
                        %subst(srcdta:38:5)= len08;
                     When wky = 9;
                        %subst(srcdta:6:30)= 'd ' + key09;
                        %subst(srcdta:38:5)= len09;
                     When wky =10;
                        %subst(srcdta:6:30)= 'd ' + key10;
                        %subst(srcdta:38:5)= len10;
                  endsl;

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;
                  wky += 1;

                enddo;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;
                wky += 1;



                %subst(srcdta:6:60) = 'd ' + %subst(protog:1:8) +
                'delete  pr';
                %subst(srcdta:40:1)= 'n';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                wky = 1;
                dow wky <= keyflds;
                  Select;
                     When wky = 1;
                        %subst(srcdta:6:30)= 'd ' + key01;
                        %subst(srcdta:38:5)= len01;
                     When wky = 2;
                        %subst(srcdta:6:30)= 'd ' + key02;
                        %subst(srcdta:38:5)= len02;
                     When wky = 3;
                        %subst(srcdta:6:30)= 'd ' + key03;
                        %subst(srcdta:38:5)= len03;
                     When wky = 4;
                        %subst(srcdta:6:30)= 'd ' + key04;
                        %subst(srcdta:38:5)= len04;
                     When wky = 5;
                        %subst(srcdta:6:30)= 'd ' + key05;
                        %subst(srcdta:38:5)= len05;
                     When wky = 6;
                        %subst(srcdta:6:30)= 'd ' + key06;
                        %subst(srcdta:38:5)= len06;
                     When wky = 7;
                        %subst(srcdta:6:30)= 'd ' + key07;
                        %subst(srcdta:38:5)= len07;
                     When wky = 8;
                        %subst(srcdta:6:30)= 'd ' + key08;
                        %subst(srcdta:38:5)= len08;
                     When wky = 9;
                        %subst(srcdta:6:30)= 'd ' + key09;
                        %subst(srcdta:38:5)= len09;
                     When wky =10;
                        %subst(srcdta:6:30)= 'd ' + key10;
                        %subst(srcdta:38:5)= len10;
                  endsl;

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;
                  wky += 1;

                enddo;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;
                wky += 1;

                %subst(srcdta:6:60) = 'd ' + %subst(protog:1:8) +
                'insert  pr';
                %subst(srcdta:40:1)= 'n';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;


         endsr;

        //------------------------------------------------------------------
        //   Build Signature Records...
        //------------------------------------------------------------------
         begsr $_bld_Symg;

           if whfldd = 0;
              wktwo = %subst(%editc(wHFLDB : 'X'):4:2);
           else;
              wktwo = %subst(%editc(wHFLDD : 'X'):1:2);
           endif;

           Select;
             when wkx = 1;
                %subst(srcdta:1:70)= setpgm + quote(1) + FileId + srvpgm +
                quote(1) + ')' ;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;


                clear srcdta;
                wkx  += 1;

                %subst(srcdta:1:70)= export + protog +
                %subst(getrcd:1:6) +  ')' ;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:1:70)= export + protog +
                %trimr(whfldi) + ')';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

             other;

                %subst(srcdta:1:70)= export + protog +
                %trimr(whfldi) +  ')' ;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

           Endsl;


         endsr;

        //------------------------------------------------------------------
        //   Build Signature Records...
        //------------------------------------------------------------------
         begsr $_bld_Syms;

                %subst(srcdta:1:70)= export + protos +
                %trimr(whfldi) +   ')' ;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;


         endsr;

        //------------------------------------------------------------------
        //   Build Database Function Symbols
        //------------------------------------------------------------------
         begsr $_bld_Dbfs;

                %subst(srcdta:1:70)= export + %subst(protos:1:8) + 'clear' +
                ')';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:1:70)= export + %subst(protos:1:8) +
                'update' +  ')';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:1:70)= export + %subst(protos:1:8) +
                'delete' +  ')';


                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:1:70)= export + %subst(protos:1:8) +
                'insert' +  ')';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:1:70)= 'ENDPGMEXP';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

         endsr;

        //------------------------------------------------------------------
        //   Build SQL Gets Service Program
        //------------------------------------------------------------------
         begsr $_bld_Sqlg;

           if whfldd = 0;
              wktwo = %subst(%editc(wHFLDB : 'X'):4:2);
           else;
              wktwo = %subst(%editc(wHFLDD : 'X'):1:2);
           endif;

           Select;
             when wkx = 1;
                clear srcdta;
                %subst(srcdta:6:70)= 'h* Get/Update/Insert ' + FileId;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= 'h nomain ';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= 'd ' + %subst(FileId:3:7);
                %subst(srcdta:22:4)= 'e ds';
                %subst(srcdta:44:8)= 'extname(';
                %subst(srcdta:52:30) = %trimr(FileId) + ') prefix(nw:2)';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Prototypes ';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' /copy ' + Filehd;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= 'd PSDS';
                %subst(srcdta:22:4)= ' sds';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= 'd  CurUserId';
                %subst(srcdta:30:3)= '358';
                %subst(srcdta:37:3)= '367';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Global Variables';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;


                %subst(srcdta:6:70)= ' * ';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *   Include any Global Variable';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= 'd SQLStateOK      c';
                %subst(srcdta:44:6)= 'Const(';
                %subst(srcdta:50:30)= quote(1) + '00000' + quote(1) +')';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;


                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Select Record and populate DS';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:60) = 'p ' + protog  + getrcd;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:24:1)= 'b';
                %subst(srcdta:44:6)= 'Export';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:60) = 'd ' + protog  + getrcd;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:24:2) = 'pi';
                %subst(srcdta:40:1) = 'n';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                wky = 1;
                dow wky <= keyflds;
                  Select;
                     When wky = 1;
                        %subst(srcdta:6:30)= 'd ' + key01;
                        %subst(srcdta:38:5)= len01;
                     When wky = 2;
                        %subst(srcdta:6:30)= 'd ' + key02;
                        %subst(srcdta:38:5)= len02;
                     When wky = 3;
                        %subst(srcdta:6:30)= 'd ' + key03;
                        %subst(srcdta:38:5)= len03;
                     When wky = 4;
                        %subst(srcdta:6:30)= 'd ' + key04;
                        %subst(srcdta:38:5)= len04;
                     When wky = 5;
                        %subst(srcdta:6:30)= 'd ' + key05;
                        %subst(srcdta:38:5)= len05;
                     When wky = 6;
                        %subst(srcdta:6:30)= 'd ' + key06;
                        %subst(srcdta:38:5)= len06;
                     When wky = 7;
                        %subst(srcdta:6:30)= 'd ' + key07;
                        %subst(srcdta:38:5)= len07;
                     When wky = 8;
                        %subst(srcdta:6:30)= 'd ' + key08;
                        %subst(srcdta:38:5)= len08;
                     When wky = 9;
                        %subst(srcdta:6:30)= 'd ' + key09;
                        %subst(srcdta:38:5)= len09;
                     When wky =10;
                        %subst(srcdta:6:30)= 'd ' + key10;
                        %subst(srcdta:38:5)= len10;
                  endsl;

                  srcseq = wkx;

                  write rk_protos;
                  srcdat = wkcdat;
                  clear srcdta;
                  wkx  += 1;
                  wky += 1;

                enddo;

                %subst(srcdta:6:30)= 'd Isfound';
                %subst(srcdta:24:2) = 's ';
                %subst(srcdta:40:1) = 'n';
                %subst(srcdta:44:9) = 'inz(*off)';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:8:30)= %subst(Protog:1:8) + wkclear;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:12:8)= 'exec sql';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:14:8)= 'Select *';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:16:25) = 'into :' + %subst(protog:1:7);

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:18:25) = 'from ' + FileId;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                exsr $_Bld_where;

                exsr $_Bld_Other;

             Other;
                exsr $_Bld_getfd;


           endsl;

         endsr;

        //------------------------------------------------------------------
        //   Build Others prototypes...
        //------------------------------------------------------------------
         begsr $_bld_other;

                  %subst(srcdta:12:50) = 'if SQLState = SQLStateOK;';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:15:50) = 'isFound = *on;';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:12:50) = 'endif;';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;


                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:10:50) = 'return isFound;';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:7:50) = '/end-free';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:6:60) = 'p ' + protog  + getrcd;

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:6:02) = 'p ';
                  %subst(srcdta:24:1)= 'e';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;


                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Clear Data Structure holding Records';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:8:60) = %subst(protog:1:8) + 'clear';
                %subst(srcdta:24:1)= 'b';
                %subst(srcdta:44:6)= 'Export';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:8:60) = %subst(protog:1:8) + 'clear';
                %subst(srcdta:24:2)= 'pi';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:8:30)= 'clear ' + %subst(Filehd:1:7) + ';';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:8:30)= 'return' + ';';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /end-free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:8:60) = %subst(protog:1:8) + 'clear';
                %subst(srcdta:24:2)= 'e ';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Get value from ' + whftxt;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:8:60) = protog + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:24:1)= 'b';
                %subst(srcdta:44:6)= 'export';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:8:60) = protog + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:24:2) = 'pi';
   63           %subst(srcdta:38:2)= wktwo;
                %subst(srcdta:40:1) = whfldt;

                if whfldt = 'P';
                %subst(srcdta:40:1)= ' ';
                   wktwod = %editc(whfldp: 'X');
                   %subst(srcdta:42:1)= %subst(wktwod:2:1);
                endif;

                if %subst(srcdta:38:1)= '0';
                   %subst(srcdta:38:1)= ' ';
                endif;

                if whfldt = 'L';
                   %subst(srcdta:38:3)= '  D';
                endif;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:8:30)= 'return ' + 'nw_' + %subst(whfldi:4:7) +
                ';';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:7:30)= '/end-free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= 'p ' + protog + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:2) = 'p ';
                %subst(srcdta:24:2)= 'e ';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;


         endsr;

        //------------------------------------------------------------------
        //   Build Where Clause...
        //------------------------------------------------------------------
         begsr $_bld_where;

                wky = keyflds;
                if wky <= keyflds;
                  Select;
                     When wky = 1;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = %trimr(nam01) + ' = ' + ':' +
                        %subst(key01:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 2;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = %trimr(nam01) + ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 3;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = nam01+ ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam03) + ' = ' + ':' +
                        %subst(key03:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 4;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = nam01+ ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam03) + ' = ' + ':' +
                        %subst(key03:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam04) + ' = ' + ':' +
                        %subst(key04:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 5;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = nam01+ ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam03) + ' = ' + ':' +
                        %subst(key03:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam04) + ' = ' + ':' +
                        %subst(key04:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam05) + ' = ' + ':' +
                        %subst(key05:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 6;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = nam01+ ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam03) + ' = ' + ':' +
                        %subst(key03:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam04) + ' = ' + ':' +
                        %subst(key04:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam05) + ' = ' + ':' +
                        %subst(key05:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam06) + ' = ' + ':' +
                        %subst(key06:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 7;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = nam01+ ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam03) + ' = ' + ':' +
                        %subst(key03:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam04) + ' = ' + ':' +
                        %subst(key04:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam05) + ' = ' + ':' +
                        %subst(key05:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam06) + ' = ' + ':' +
                        %subst(key06:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam07) + ' = ' + ':' +
                        %subst(key07:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 8;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = %trimr(nam01) + ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam03) + ' = ' + ':' +
                        %subst(key03:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam04) + ' = ' + ':' +
                        %subst(key04:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam05) + ' = ' + ':' +
                        %subst(key05:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam06) + ' = ' + ':' +
                        %subst(key06:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam07) + ' = ' + ':' +
                        %subst(key07:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam08) + ' = ' + ':' +
                        %subst(key08:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 9;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = nam01+ ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam03) + ' = ' + ':' +
                        %subst(key03:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam04) + ' = ' + ':' +
                        %subst(key04:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam05) + ' = ' + ':' +
                        %subst(key05:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam06) + ' = ' + ':' +
                        %subst(key06:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam07) + ' = ' + ':' +
                        %subst(key07:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam08) + ' = ' + ':' +
                        %subst(key08:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam09) + ' = ' + ':' +
                        %subst(key09:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                     When wky = 10;
                        %subst(srcdta:20:06) = 'Where ';
                        %subst(srcdta:26:27) = nam01+ ' = ' + ':' +
                        %subst(key01:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam02) + ' = ' + ':' +
                        %subst(key02:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam03) + ' = ' + ':' +
                        %subst(key03:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam04) + ' = ' + ':' +
                        %subst(key04:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam05) + ' = ' + ':' +
                        %subst(key05:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam06) + ' = ' + ':' +
                        %subst(key06:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam07) + ' = ' + ':' +
                        %subst(key07:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam08) + ' = ' + ':' +
                        %subst(key08:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam09) + ' = ' + ':' +
                        %subst(key09:2:7) + ' and';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                        %subst(srcdta:22:50) = %trimr(nam10) + ' = ' + ':' +
                        %subst(key10:2:7) + ';';

                        srcseq = wkx;
                        srcdat = wkcdat;
                        write rk_protos;

                        clear srcdta;
                        wkx  += 1;

                  endsl;

                  wky  += 1;

                endif;

         endsr;

        //------------------------------------------------------------------
        //   Build SQL Get specifi value...
        //------------------------------------------------------------------
         begsr $_bld_Getfd;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Get value from ' + whftxt;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:8:60) = protog + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:24:1)= 'b';
                %subst(srcdta:44:6)= 'export';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:8:60) = protog + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:24:2) = 'pi';
   63           %subst(srcdta:38:2)= wktwo;
                %subst(srcdta:40:1) = whfldt;

                if whfldt = 'P';
                %subst(srcdta:40:1)= ' ';
                   wktwod = %editc(whfldp: 'X');
                   %subst(srcdta:42:1)= %subst(wktwod:2:1);
                endif;

                if %subst(srcdta:38:1)= '0';
                   %subst(srcdta:38:1)= ' ';
                endif;

                if whfldt = 'L';
                   %subst(srcdta:38:3)= '  D';
                endif;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:8:30)= 'return ' + 'nw_' + %subst(whfldi:4:7) +
                ';';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:7:30)= '/end-free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= 'p ' + protog + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:2) = 'p ';
                %subst(srcdta:24:2)= 'e ';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

         endsr;

        //------------------------------------------------------------------
        //   Build SQL Sets Service Program
        //------------------------------------------------------------------
         begsr $_bld_Sqls;

                exsr $_Bld_setfd;

         endsr;

        //------------------------------------------------------------------
        //   Build SQL Sets Fields...
        //------------------------------------------------------------------
         begsr $_bld_Setfd;

                if whfldd = 0;
                   wktwo = %subst(%editc(wHFLDB : 'X'):4:2);
                else;
                   wktwo = %subst(%editc(wHFLDD : 'X'):1:2);
                endif;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Set value for ' + whftxt;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:8:60) = protos + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:24:1)= 'b';
                %subst(srcdta:44:6)= 'export';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:8:60) = protos + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:24:2) = 'pi';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

   63           %subst(srcdta:6:25)= 'd Pm' + %subst(whfldi:4:7);
   63           %subst(srcdta:38:2)= wktwo;
                %subst(srcdta:40:1) = whfldt;
                %subst(srcdta:44:6)= 'const ';

                if whfldt = 'P';
                %subst(srcdta:40:1)= ' ';
                   wktwod = %editc(whfldp: 'X');
                   %subst(srcdta:42:1)= %subst(wktwod:2:1);
                endif;

                if %subst(srcdta:38:1)= '0';
                   %subst(srcdta:38:1)= ' ';
                endif;

                if whfldt = 'L';
                   %subst(srcdta:38:3)= '  D';
                endif;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:8:30)= 'nw_' + %subst(whfldi:4:7) +
                ' = ' + 'Pm' + %subst(whfldi:4:7) + ';';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:8:30)= 'return;';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:7:30)= '/end-free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= 'p ' + protos + %trimr(whfldi) +
                '...';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:2) = 'p ';
                %subst(srcdta:24:2)= 'e ';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

         endsr;

        //------------------------------------------------------------------
        //   Build DEL Statement...
        //------------------------------------------------------------------
         begsr $_bld_Dels;

                %subst(srcdta:6:70)= ' *' + dashes;


                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Select record for Delete.. DS';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'p ';
                %subst(srcdta:8:30)= %subst(Protog:1:8) + wkdelete;
                %subst(srcdta:24:1)= 'b';
                %subst(srcdta:44:6)= 'Export';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:02) = 'd ';
                %subst(srcdta:8:30)= %subst(Protog:1:8) + wkdelete;
                %subst(srcdta:24:2)= 'pi';
                %subst(srcdta:40:1) = 'n';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                Select;
                     When wky = 1;
                        %subst(srcdta:6:30)= 'd ' + key01;
                        %subst(srcdta:38:5)= len01;
                     When wky = 2;
                        %subst(srcdta:6:30)= 'd ' + key02;
                        %subst(srcdta:38:5)= len02;
                     When wky = 3;
                        %subst(srcdta:6:30)= 'd ' + key03;
                        %subst(srcdta:38:5)= len03;
                     When wky = 4;
                        %subst(srcdta:6:30)= 'd ' + key04;
                        %subst(srcdta:38:5)= len04;
                     When wky = 5;
                        %subst(srcdta:6:30)= 'd ' + key05;
                        %subst(srcdta:38:5)= len05;
                     When wky = 6;
                        %subst(srcdta:6:30)= 'd ' + key06;
                        %subst(srcdta:38:5)= len06;
                     When wky = 7;
                        %subst(srcdta:6:30)= 'd ' + key07;
                        %subst(srcdta:38:5)= len07;
                     When wky = 8;
                        %subst(srcdta:6:30)= 'd ' + key08;
                        %subst(srcdta:38:5)= len08;
                     When wky = 9;
                        %subst(srcdta:6:30)= 'd ' + key09;
                        %subst(srcdta:38:5)= len09;
                     When wky =10;
                        %subst(srcdta:6:30)= 'd ' + key10;
                        %subst(srcdta:38:5)= len10;
                endsl;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= 'd IsDeleted';
                %subst(srcdta:24:2) = 's ';
                %subst(srcdta:40:1) = 'n';
                %subst(srcdta:44:9) = 'inz(*off)';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:12:8)= 'exec sql';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:14:15) = 'Delete from ';
                %subst(srcdta:26:10) = FileId;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                exsr $_Bld_where;

                  %subst(srcdta:12:50) = 'if SQLState = SQLStateOK;';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:15:50) = 'isDeleted = *on;';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:12:50) = 'endif;';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;


                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:10:50) = 'return isDeleted;';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:7:50) = '/end-free';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:6:60) = 'p ' + %subst(protog:1:8) +
                  wkdelete;
                  %subst(srcdta:24:1)= 'e';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

         endsr;


        //------------------------------------------------------------------
        //   Build UPD Statement...
        //------------------------------------------------------------------
         begsr $_bld_Upds;

           Select;
             when wkx = 2000;
                %subst(srcdta:6:70)= ' *' + dashes;


                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Select record for Update... DS    *';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:60) = 'p ' + %subst(protog:1:8) +
                'update  b ';
                %subst(srcdta:44:6)= 'Export';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:60) = 'd ' + %subst(protog:1:8) +
                'update  pi';
                %subst(srcdta:40:1) = 'n';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                wky = 1;
                dow wky <= keyflds;
                  Select;
                     When wky = 1;
                        %subst(srcdta:6:30)= 'd ' + key01;
                        %subst(srcdta:38:5)= len01;
                     When wky = 2;
                        %subst(srcdta:6:30)= 'd ' + key02;
                        %subst(srcdta:38:5)= len02;
                     When wky = 3;
                        %subst(srcdta:6:30)= 'd ' + key03;
                        %subst(srcdta:38:5)= len03;
                     When wky = 4;
                        %subst(srcdta:6:30)= 'd ' + key04;
                        %subst(srcdta:38:5)= len04;
                     When wky = 5;
                        %subst(srcdta:6:30)= 'd ' + key05;
                        %subst(srcdta:38:5)= len05;
                     When wky = 6;
                        %subst(srcdta:6:30)= 'd ' + key06;
                        %subst(srcdta:38:5)= len06;
                     When wky = 7;
                        %subst(srcdta:6:30)= 'd ' + key07;
                        %subst(srcdta:38:5)= len07;
                     When wky = 8;
                        %subst(srcdta:6:30)= 'd ' + key08;
                        %subst(srcdta:38:5)= len08;
                     When wky = 9;
                        %subst(srcdta:6:30)= 'd ' + key09;
                        %subst(srcdta:38:5)= len09;
                     When wky =10;
                        %subst(srcdta:6:30)= 'd ' + key10;
                        %subst(srcdta:38:5)= len10;
                  endsl;

                  wky +=1;

                enddo;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= 'd IsUpdated';
                %subst(srcdta:24:2) = 's ';
                %subst(srcdta:40:1) = 'n';
                %subst(srcdta:44:9) = 'inz(*off)';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:12:8)= 'exec sql';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:14:25) = 'update ' + Fileid;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:16:4) = 'set ';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                wky   = 1;
                wki   = 1;
                wkv   = 1;
                Upf01(wky) = whfldi + '=' + ':nw' + %subst(whfldi:3:8);
                Upc01(wky) = ',';
                wki  += 1;
                wkv  += 1;

             other;
               Select;
                  When wki <= 3;
                     Select;
                        When wki = 2;
                            Upf02(wky) = whfldi + '=' + ':nw' +
                            %subst(whfldi:3:8);
                            Upc02(wky) = ',';
                            wki  += 1;
                            wkv  += 1;
                        When wki = 3;
                            Upf03(wky) = whfldi + '=' + ':nw' +
                            %subst(whfldi:3:8);
                            wki  += 1;
                            wkv  += 1;
                     Endsl;
                  Other;
                      wky  += 1;
                      wki   = 1;
                      wkv   = 1;
                Upf01(wky) = whfldi + '=' + ':nw' + %subst(whfldi:3:8);
                Upc01(wky) = ',';
                wki  += 1;
                wkv  += 1;
               endsl;

           endsl;

         endsr;

        //------------------------------------------------------------------
        //   Build INS Statement...
        //------------------------------------------------------------------
         begsr $_bld_Inss;

           Select;
             when wkx = 3000;
                %subst(srcdta:6:70)= ' *' + dashes;


                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' * Select record for Write... DS     *';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:70)= ' *' + dashes;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:60) = 'p ' + %subst(protog:1:8) +
                'insert  b ';
                %subst(srcdta:44:6)= 'Export';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:60) = 'd ' + %subst(protog:1:8) +
                'insert  pi';
                %subst(srcdta:40:1) = 'n';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= 'd IsWritten';
                %subst(srcdta:24:2) = 's ';
                %subst(srcdta:40:1) = 'n';
                %subst(srcdta:44:9) = 'inz(*off)';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:6:30)= ' /free';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:12:8)= 'exec sql';

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                %subst(srcdta:14:25) = 'insert into ' + FIleid;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                srcseq = wkx;
                srcdat = wkcdat;
                write rk_protos;

                clear srcdta;
                wkx  += 1;

                wky   = 1;
                wki   = 1;
                wkv   = 1;
                Inf01(wky) = whfldi;
                Inc01(wky) = ',';
                vaf01(wky) = ':nw' + %subst(whfldi:3:8);
                vac01(wky) = ',';
                wki  += 1;
                wkv  += 1;

             other;
               Select;
                  When wki <= 5;
                     Select;
                        When wki = 2;
                            Inf02(wky) = whfldi;
                            Inc02(wky) = ',';
                            vaf02(wky) = ':nw' + %subst(whfldi:3:8);
                            vac02(wky) = ',';
                            wki  += 1;
                            wkv  += 1;
                        When wki = 3;
                            Inf03(wky) = whfldi;
                            Inc03(wky) = ',';
                            vaf03(wky) = ':nw' + %subst(whfldi:3:8);
                            vac03(wky) = ',';
                            wki  += 1;
                            wkv  += 1;
                        When wki = 3;
                            Inf03(wky) = whfldi;
                            Inc03(wky) = ',';
                            vaf03(wky) = ':nw' + %subst(whfldi:3:8);
                            vac03(wky) = ',';
                            wki  += 1;
                            wkv  += 1;
                        When wki = 4;
                            Inf04(wky) = whfldi;
                            Inc04(wky) = ',';
                            vaf04(wky) = ':nw' + %subst(whfldi:3:8);
                            vac04(wky) = ',';
                            wki  += 1;
                            wkv  += 1;
                        When wki = 5;
                            Inf05(wky) = whfldi;
                            Inc05(wky) = ',';
                            vaf05(wky) = ':nw' + %subst(whfldi:3:8);
                            vac05(wky) = ',';
                            wki  += 1;
                            wkv  += 1;
                     Endsl;
                  Other;
                      wky  += 1;
                      wki   = 1;
                      wkv   = 1;
                Inf01(wky) = whfldi;
                Inc01(wky) = ',';
                vaf01(wky) = ':nw' + %subst(whfldi:3:8);
                vac01(wky) = ',';
                wki  += 1;
                wkv  += 1;
               endsl;

           Endsl;

         endsr;

        //------------------------------------------------------------------
        //   Write UPD Statement...
        //------------------------------------------------------------------
         begsr $_wrt_Upds;

           wky = 1;
           dow  wky <= 99;
            if ArrUpd(wky) = *blanks;

             srcseq = wkx;
             srcdat = wkcdat;
             write rk_protos;

             clear srcdta;
             wkx  += 1;
             leave;
            endif;

            %subst(srcdta: 8:72) = ArrUpd(wky);
            wky += 1;

            srcseq = wkx;
            srcdat = wkcdat;
            write rk_protos;

            clear srcdta;
            wkx  += 1;

           enddo;

           exsr $_bld_where;

           %subst(srcdta:12:50) = 'if SQLState = SQLStateOK;';

           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;

           %subst(srcdta:15:50) = 'isUpdated = *on;';

           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;

           %subst(srcdta:12:50) = 'endif;';

           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;


           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;

           %subst(srcdta:10:50) = 'return isUpdated;';

           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;

           %subst(srcdta:7:50) = '/end-free';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:6:60) = 'p ' + %subst(protog:1:8) +
                  'update  e';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;


         endsr;

        //------------------------------------------------------------------
        //   Write INS Statement...
        //------------------------------------------------------------------
         begsr $_wrt_Inss;

           %subst(srcdta:16:1) = '(';
           wky = 1;
           dow  wky <= 99;
            if Arrins(wky) = *blanks;
             %subst(srcdta:16:1) = ')';
             srcseq = wkx;
             srcdat = wkcdat;
             write rk_protos;

             clear srcdta;
             wkx  += 1;
             leave;
            endif;

            %subst(srcdta:17:55) = ArrIns(wky);
            wky += 1;

            srcseq = wkx;
            srcdat = wkcdat;
            write rk_protos;

            clear srcdta;
            wkx  += 1;

           enddo;

           %subst(srcdta:16:7) = 'values(';
            srcseq = wkx;
            srcdat = wkcdat;
            write rk_protos;

            clear srcdta;
            wkx  += 1;

           wky = 1;
           dow  wky <= 99;
            if Arrval(wky) = *blanks;
             %subst(srcdta:16:1) = ')';

             srcseq = wkx;
             srcdat = wkcdat;
             write rk_protos;

             clear srcdta;
             wkx  += 1;

             leave;
            endif;

            %subst(srcdta:23:60) = ArrVal(wky);
            wky += 1;

            srcseq = wkx;
            srcdat = wkcdat;
            write rk_protos;

            clear srcdta;
            wkx  += 1;

           enddo;

           %subst(srcdta:12:50) = 'if SQLState = SQLStateOK;';

           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;

           %subst(srcdta:15:50) = 'isWritten = *on;';

           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;

           %subst(srcdta:12:50) = 'endif;';

           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;


           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;

           %subst(srcdta:10:50) = 'return isWritten;';

           srcseq = wkx;
           srcdat = wkcdat;
           write rk_protos;

           clear srcdta;
           wkx  += 1;

           %subst(srcdta:7:50) = '/end-free';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

                  %subst(srcdta:6:60) = 'p ' + %subst(protog:1:8) +
                  'insert  e';

                  srcseq = wkx;
                  srcdat = wkcdat;
                  write rk_protos;

                  clear srcdta;
                  wkx  += 1;

         endsr;

      /end-free
** Cmdary
'
