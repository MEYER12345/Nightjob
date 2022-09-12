       ctl-opt dftactgrp(*no);
       dcl-f k_prodsoqa;
       dcl-f dmtestlstd printer oflind(fullpage);
       dcl-s fullpage Ind Inz(*on);

       dcl-s first15 char(15);

      /free
       setll *start k_prodsoqa;
       //Test comment
       read k_prodsoqa;
       dou %eof(k_prodsoqa);
          if fullpage;
             write heading; 
             clear fullpage;
          endif;
          write detail;
          read k_prodsoqa;
       enddo;
       *inlr = *on;
      /end-free
