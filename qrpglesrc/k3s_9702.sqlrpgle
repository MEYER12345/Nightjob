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
      **   Name: K3S_9702
      **   Type: ILE RPG Program
      **   Desc: Users batch job to K3S-Replenish end job *IMMED
      **
      *****************************************************************
      **
      **  This program is used to end the batch jobs in K3S-Replenish
      **
      *****************************************************************
      **
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/29/2014.
      **  Remarks. Altered program to utilize a cursor and embedded
      **           SQL to read through K_COMPANY records. Also,
      **           changed to utilize /COPY to use prototypes for
      **           program calls.
      **
      *****************************************************************
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_9702        PI
     d  pgmnot                       10
      * --------------------------------------------------- parameter passed
      * array of APP program names to NOT remove
     d pgmary          s             10a   dim(50)                              valid deal numbers
     d x               s              2  0 inz(0)
     d lkup            s              2  0
     d index           s              4  0
     d nbrusers        s              4  0                                      location
     d #codetyp        s              3
     d endoption       s              1a
     d pgmnotapp1      s             10
     d pgmnotapp2      s             10
     d spaceattr       s             10
     d spacevalue      s              1
     d spacetext       s             50
     d spaceauth       s             10    inz('*ALL')
     d spacereplc      s             10    inz('*YES')
     d spacelen        s              9b 0 inz(1024)
     d fileformat      s              8    inz('JOBL0100')
     d filelib         s             26    inz('K3S_5*    *ALL      *ALL  ')
     d objecttype      s             10    inz('*ALL      ')
     d membername      s             10    inz('*NONE     ')
     d startposit      s              9b 0
     d startlen        s              9b 0
     d savestart       s              9b 0
     d savelen         s              9b 0
     d status          s              7
     d errords         ds                  inz
     d bytesprovd              1      4b 0 inz(116)
     d bytesavail              5      8b 0
     d messageid               9     15
     d err###                 16     16
     d messagedta             17    116
     d inputds         ds                  inz
     d userspace                     20
     d spacename                     10    overlay(userspace:1)
     d                                     inz('ACTIVEJOBS')
     d spacelib                      10    overlay(userspace:11)
     d                                     inz('QTEMP')
     d genheadds       ds                  inz
     D inptoffset            117    120b 0
     d inputsize             121    124b 0
     d listoffset            125    128b 0
     d numberlist            133    136b 0
     d entrysize             137    140b 0
     d listds          ds
     d jobname                 1     10
     d jobuser                11     20
     d job#                   21     26
     d spaces                 27     42
     d jobstatus              43     52
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d CRTUSRSPC       PR                  EXTPGM('QUSCRTUS')
     d  userspace                          like(userspace)
     d  spaceattr                          like(spaceattr)
     d  spacelen                           like(spacelen)
     d  spacevalue                         like(spacevalue)
     d  spaceauth                          like(spaceauth)
     d  spacetext                          like(spacetext)
     d  spacereplc                         like(spacereplc)
     d  errords                            like(errords)
     d*
     d CRTACTUSL       PR                  EXTPGM('QUSLJOB')
     d  userspace                          like(userspace)
     d  fileformat                         like(fileformat)
     d  filelib                            like(filelib)
     d  objecttype                         like(objecttype)
     d  errords                            like(errords)
     d*
     d RTVHDRINFO      PR                  EXTPGM('QUSRTVUS')
     d  userspace                          like(userspace)
     d  startposit                         like(startposit)
     d  startlen                           like(startlen)
     d  genheadds                          like(genheadds)
     d*
     d RTVDTLINFO      PR                  EXTPGM('QUSRTVUS')
     d  userspace                          like(userspace)
     d  startposit                         like(startposit)
     d  startlen                           like(startlen)
     d  inputds                            like(inputds)
     d*
     d RTVDTINLOG      PR                  EXTPGM('QUSRTVUS')
     d  userspace                          like(userspace)
     d  startposit                         like(startposit)
     d  startlen                           like(startlen)
     d  listds                             like(listds)
     d*
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      *
     d company_rec   e ds                  ExtName(k_company)
     d tablcod_rec   e ds                  ExtName(k_tablcod)
      * ------------------------------------------------------- Once Routine
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;
       //once routine
       exsr $_once;

       //-------------------------------------------------- End of Main Loop

       //finished set on LR
       *inlr = *on;

       //**************************************************** End of program

       //////////////////////////////////////////////////////// Once routine

       begsr $_once;

       exsr dcltacursor;

       exsr dclcmcursor;
       exsr clscmcursor;
       exsr opncmcursor;

       x = *zeros;
       pgmary = *blanks;

       dow SQLState = SQLStateOk;

         exec sql
           fetch next
           from cmcursor
           into :company_rec;

         if SQLState = RowNotFound;
           leave;
         endif;

         #codetyp = 'APP';
         exsr inzInpSrch;
         exsr intSQLStmt;
         exsr prepDynSQLStmt;
         if SQLState = SQLStateOk;   //Good prep

            exsr opntacursor;

            dow SQLState = SQLStateOk;

               exec sql
                fetch next
                 from tacursor
                 into :tablcod_rec;

               if SQLState = RowNotFound;
                  Leave;
               endif;

               pgmnotapp1 = ta_codeval;
               evalr pgmnotapp2 = ta_codeval;
               if pgmnotapp1 = 'K3S_9702  ';
                  if %lookup (pgmnotapp2:pgmary) = 0;
                     x = x + 1;
                     pgmary(x) = pgmnotapp2;
                  endif;
               endif;
            enddo;
            exsr clstacursor;
         endif;
       enddo;

       exsr clscmcursor;

       //Creat user space in qtemp called ACTIVEJOBS
       spaceattr = *blanks;
       spacevalue = *blanks;
       spacetext = *blanks;
       callp CRTUSRSPC (userspace:
                        spaceattr:
                        spacelen:
                        spacevalue:
                        spaceauth:
                        spacetext:
                        spacereplc:
                        errords);

        //Create active user list in ACTIVEJOBS for K3S_5*
       callp CRTACTUSL (userspace:
                        fileformat:
                        filelib:
                        objecttype:
                        errords);

       spacename = 'ACTIVEJOBS';

       spacelib   ='QTEMP     ';

       //retrieve for user area header information
       startposit = 1;
       startlen = 140;
       callp RTVHDRINFO (userspace:
                         startposit:
                         startlen:
                         genheadds);
       startlen = inputsize;
       spacename = 'ACTIVEJOBS';
       spacelib = 'QTEMP';

       //retrieve for user detail information
       startposit = 1;
       callp RTVDTLINFO (userspace:
                         startposit:
                         startlen:
                         inputds);
       startposit = (listoffset +1);
       startlen = entrysize;
       savelen = entrysize;
       spacename = 'ACTIVEJOBS';
       spacelib ='QTEMP     ';
       nbrusers = numberlist;

       //retrieve for user area detail information for all logged entries
       For index = 1 to nbrusers;
           spacename = 'ACTIVEJOBS';
           spacelib ='QTEMP     ';
           savestart = startposit;
           callp RTVDTINLOG (userspace:
                             startposit:
                             startlen:
                             listds);

           lkup = %lookup(jobname:pgmary);

           status = jobstatus;
           if status = '*ACTIVE' and
                       jobname <> pgmnot and
                       lkup = 0 and
                       jobname <> 'K3S_RMVJOB' or
                       status = '*JOBQ  ' and
                       jobname <> pgmnot and
                       lkup = 0 and
                       jobname <> 'K3S_RMVJOB';
                       callp K3S_9702CL (jobname:
                                         jobuser:
                                         job#);
           endif;
           startposit = (savestart + savelen);
       endfor;
       endsr;

       begsr dclcmcursor;
       exec sql
        declare cmcursor Cursor
         for
         select *
         from k_company
         order by cm_comp;
       endsr;

       begsr opncmcursor;
       exec sql
        open cmcursor;
        if SQLState <> SQLStateOk;
           exsr clscmcursor;
           exec sql
            open cmcursor;
        endif;
       endsr;

       begsr clscmcursor;
       exec sql
        close cmcursor;
       endsr;

       begsr clstacursor;
       exec sql
        close tacursor;
       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_tablcod +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'ta_comp = ? and +
                     ta_codetyp = ? +
                     Order by ta_comp, +
                              ta_codetyp, +
                              ta_codeval';
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr dcltacursor;
       exec sql
        declare tacursor Cursor
         for DynSQLStmt;
       endsr;

       begsr opntacursor;
       exec sql
        open tacursor
          using :cm_comp,
                :#codetyp;
       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;
      /end-free

