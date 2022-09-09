CREATE OR REPLACE PACKAGE DCG_PK_4STORE AS
  v_stop                            INTEGER          :=23;
  v_test                            boolean          :=false;
  v_preprod                         boolean          :=false;
  v_timeout                         INTEGER          :=300;
  v_dettagli                        VARCHAR2(32767);
  v_obj_pref                        VARCHAR2(10)     :='';
  v_finestra_retention_gg_scn       integer          :=30;
  v_finestra_retention_gg_fc        integer          :=30;
  v_finestra_retention_gg_fatcli    integer          :=30;
  
/******************************************************************************
     NAME:       DCG_PR_SCONTRINI
     PURPOSE:    Procedura che attiva i trigger dei servizi REST(GET) per gli SCONTRINI

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena
  *******************************************************************************/
PROCEDURE DCG_TRIGGER_SCONTRINI   (p_job IN VARCHAR2,p_period IN VARCHAR2
                                ,p3 IN VARCHAR2 DEFAULT NULL
                                ,p4 IN VARCHAR2 DEFAULT NULL
                                ,p5 IN VARCHAR2 DEFAULT NULL
                                ,p6 IN VARCHAR2 DEFAULT NULL
                                ,p7 IN VARCHAR2 DEFAULT NULL
                                ,p8 IN VARCHAR2 DEFAULT NULL
                                ,p9 IN VARCHAR2 DEFAULT NULL
                               ,p10 IN VARCHAR2 DEFAULT NULL
                               ,p11 IN VARCHAR2 DEFAULT NULL);
/******************************************************************************
     NAME:       DCG_PR_SCONTRINI
     PURPOSE:    Procedura che attiva i trigger dei servizi REST(GET) per i FOGLICASSA

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena
  *******************************************************************************/                               
PROCEDURE DCG_TRIGGER_FOGLICASSA  (p_job IN VARCHAR2,p_period IN VARCHAR2
                                ,p3 IN VARCHAR2 DEFAULT NULL
                                ,p4 IN VARCHAR2 DEFAULT NULL
                                ,p5 IN VARCHAR2 DEFAULT NULL
                                ,p6 IN VARCHAR2 DEFAULT NULL
                                ,p7 IN VARCHAR2 DEFAULT NULL
                                ,p8 IN VARCHAR2 DEFAULT NULL
                                ,p9 IN VARCHAR2 DEFAULT NULL
                               ,p10 IN VARCHAR2 DEFAULT NULL
                               ,p11 IN VARCHAR2 DEFAULT NULL);      
/******************************************************************************
     NAME:       DCG_PR_SCONTRINI
     PURPOSE:    Procedura che attiva i trigger dei servizi REST(GET) per le FATCLI

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena
  *******************************************************************************/                                                        
PROCEDURE DCG_TRIGGER_FATCLI      (p_job IN VARCHAR2,p_period IN VARCHAR2
                                ,p3 IN VARCHAR2 DEFAULT NULL
                                ,p4 IN VARCHAR2 DEFAULT NULL
                                ,p5 IN VARCHAR2 DEFAULT NULL
                                ,p6 IN VARCHAR2 DEFAULT NULL
                                ,p7 IN VARCHAR2 DEFAULT NULL
                                ,p8 IN VARCHAR2 DEFAULT NULL
                                ,p9 IN VARCHAR2 DEFAULT NULL
                               ,p10 IN VARCHAR2 DEFAULT NULL
                               ,p11 IN VARCHAR2 DEFAULT NULL);
PROCEDURE pulizia_scontrini;
PROCEDURE pulizia_foglicassa;
PROCEDURE pulizia_fatcli;

END DCG_PK_4STORE;

/


CREATE OR REPLACE PACKAGE BODY DCG_PK_4STORE IS
  /******************************************************************************
     NAME:       DCG_TRIGGER_SCONTRINI
     PURPOSE:    Procedura che attiva i trigger dei servizi REST(GET) per gli SCONTRINI

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena Attivazione periodica del trigger http per il servizio GET degli scontrini
  *******************************************************************************/
  PROCEDURE DCG_TRIGGER_SCONTRINI(p_job     IN VARCHAR2,
                               p_period IN VARCHAR2,
                               p3       IN VARCHAR2 DEFAULT NULL,
                               p4       IN VARCHAR2 DEFAULT NULL,
                               p5       IN VARCHAR2 DEFAULT NULL,
                               p6       IN VARCHAR2 DEFAULT NULL,
                               p7       IN VARCHAR2 DEFAULT NULL,
                               p8       IN VARCHAR2 DEFAULT NULL,
                               p9       IN VARCHAR2 DEFAULT NULL,
                               p10      IN VARCHAR2 DEFAULT NULL,
                               p11      IN VARCHAR2 DEFAULT NULL) IS

    k_error_marker CONSTANT CHAR(22)     := '[*** !!! ***]  [ERROR]';
    v_unit                  VARCHAR2(30) := 'DCG_TRIGGER_SCONTRINI';
    NUM_GRUPPO_DEST         VARCHAR2(10) := '1001';   --mailing list delle notifiche 4STORE
    PID                     NUMBER;
    v_testo                 VARCHAR2(200);
    n_num_righe             INTEGER;
    
    l_http_request          UTL_HTTP.req;
    l_http_response         UTL_HTTP.resp;
    l_response_text         VARCHAR2(32000);
    l_url                   varchar2(1000):='http://172.24.28.139:3001/scontrini/';
    l_method                varchar2(100) :='GET';
    l_protocol              varchar2(100) :='HTTP/1.1';
    
    v_control               INTEGER;
    v_count                 INTEGER          :=0;
		v_start                 INTEGER          :=0;
    v_processato            VARCHAR2(1)      :='N';
    v_loop                  boolean          :=true;
    
    crlf   CONSTANT VARCHAR2(2) := CHR(13) || CHR(10);
    cr     CONSTANT VARCHAR2(2) := CHR(13);
    lf     CONSTANT VARCHAR2(2) := CHR(10);
		
    
  BEGIN
   DMS_COM.write_jlt(v_unit,1,'Inizio loop '||v_unit);
    select to_number(to_char(sysdate,'HH24'))
    into v_control
    from dual;
   --ciclo continuo dall' ora di inizio job all' ora v_stop 
   while ((v_control < v_stop) and (v_loop)) loop
       begin
           --se v_test:=true interrompiamo il ciclo
           if(v_test) then
             v_loop:=not v_loop;
           end if;
         
           DMS_LAT.step_start('Start '||v_unit, 'IN', 'Start '||v_unit);
            DMS_COM.write_jlt(v_unit, 1, 'Start '||v_unit);

            -- preparing request
            l_http_request := UTL_HTTP.begin_request(l_url,
                                                     l_method,
                                                     l_protocol);

            l_http_response := UTL_HTTP.get_response(l_http_request);
            
            UTL_HTTP.read_text(l_http_response, l_response_text);
            
            --DBMS_OUTPUT.put_line(l_response_text);
            DMS_COM.write_jlt(v_unit, 1, replace(replace(l_response_text,chr(10),' '),chr(13),' ') );
            
            UTL_HTTP.end_response(l_http_response);
            
            --retention delle tabelle di destinazione
            

            DMS_COM.write_jlt(v_unit, 1, 'End '||v_unit);
           DMS_LAT.step_end(n_num_righe, '-OK');
          
          pulizia_scontrini();
            
          dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
          --ricarico la condizione di controllo del loop esterno
          select to_number(to_char(sysdate,'HH24'))
          into v_control
          from dual;
      exception 
        WHEN UTL_HTTP.end_of_body THEN
           UTL_HTTP.end_response(l_http_response);
           DMS_COM.write_jlt(v_unit, 1, l_response_text); 
           dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
            --ricarico la condizione di controllo del loop esterno
           select to_number(to_char(sysdate,'HH24'))
           into v_control
           from dual; 
        WHEN OTHERS THEN
           DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
           --mail di notifica errori
           pid     := null;                                     
           pid     := testo_mail('La Trasmissione degli scontrini e'' andata in eccezione.', pid); 
           pid     := testo_mail('', pid);     
           pid     := testo_mail('Generata eccezione ' || SQLCODE || ' ' || SQLERRM, pid);   
           SEND_MAIL('Errore Trasmissione SCN da 4STORE', PID, NUM_GRUPPO_DEST);
           --ripresa del loop
           dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
            --ricarico la condizione di controllo del loop esterno
           select to_number(to_char(sysdate,'HH24'))
           into v_control
           from dual; 
      end;
   end loop;
	 --FINE LOOP DI CONTROLLO
	 
   DMS_COM.write_jlt(v_unit,1,'Fine Controllo loop '||v_unit);

  EXCEPTION
    WHEN UTL_HTTP.end_of_body THEN
      UTL_HTTP.end_response(l_http_response);
      DMS_COM.write_jlt(v_unit, 1, l_response_text);
    WHEN OTHERS THEN  
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DCG_TRIGGER_SCONTRINI;
  /******************************************************************************
     NAME:       DCG_TRIGGER_FOGLICASSA
     PURPOSE:    Procedura che attiva i trigger dei servizi REST(GET) per i FOGLICASSA

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena Attivazione periodica del trigger http per il servizio GET dei fogli cassa di fine giornata
  *******************************************************************************/
  PROCEDURE DCG_TRIGGER_FOGLICASSA(p_job     IN VARCHAR2,
                               p_period IN VARCHAR2,
                               p3       IN VARCHAR2 DEFAULT NULL,
                               p4       IN VARCHAR2 DEFAULT NULL,
                               p5       IN VARCHAR2 DEFAULT NULL,
                               p6       IN VARCHAR2 DEFAULT NULL,
                               p7       IN VARCHAR2 DEFAULT NULL,
                               p8       IN VARCHAR2 DEFAULT NULL,
                               p9       IN VARCHAR2 DEFAULT NULL,
                               p10      IN VARCHAR2 DEFAULT NULL,
                               p11      IN VARCHAR2 DEFAULT NULL) IS

    k_error_marker CONSTANT CHAR(22)     := '[*** !!! ***]  [ERROR]';
    v_unit                  VARCHAR2(30) := 'DCG_TRIGGER_FC';
    NUM_GRUPPO_DEST         VARCHAR2(10) := '1001';   --mailing list delle notifiche 4STORE
    PID                     NUMBER;
    v_testo                 VARCHAR2(200);
    n_num_righe             INTEGER;
    
    l_http_request          UTL_HTTP.req;
    l_http_response         UTL_HTTP.resp;
    l_response_text         VARCHAR2(32000);
    l_url                   varchar2(1000):='http://172.24.28.139:3002/foglicassa/';
    l_method                varchar2(100) :='GET';
    l_protocol              varchar2(100) :='HTTP/1.1';
    
    v_control               INTEGER;
    v_stop_fc               INTEGER:=2340;
    v_count                 INTEGER          :=0;
		v_start                 INTEGER          :=0;
    v_processato            VARCHAR2(1)      :='N';
    v_loop                  boolean          :=true;
    
    crlf   CONSTANT VARCHAR2(2) := CHR(13) || CHR(10);
    cr     CONSTANT VARCHAR2(2) := CHR(13);
    lf     CONSTANT VARCHAR2(2) := CHR(10);
    
  BEGIN
   DMS_COM.write_jlt(v_unit,1,'Inizio loop '||v_unit);
    select to_number(to_char(sysdate,'HH24MI'))
    into v_control
    from dual;
   --ciclo continuo dall' ora di inizio job all' ora v_stop 
   while ((v_control < v_stop_fc) and (v_loop)) loop
      begin       
             --se v_test:=true interrompiamo il ciclo
             if(v_test) then
               v_loop:=not v_loop;
             end if;
           
             DMS_LAT.step_start('Start '||v_unit, 'IN', 'Start '||v_unit);
              DMS_COM.write_jlt(v_unit, 1, 'Start '||v_unit);

              -- preparing request
              l_http_request := UTL_HTTP.begin_request(l_url,
                                                       l_method,
                                                       l_protocol);

              l_http_response := UTL_HTTP.get_response(l_http_request);
              
              UTL_HTTP.read_text(l_http_response, l_response_text);
              
              --DBMS_OUTPUT.put_line(l_response_text);
              DMS_COM.write_jlt(v_unit, 1, replace(replace(l_response_text,chr(10),' '),chr(13),' ') );
              
              UTL_HTTP.end_response(l_http_response);

              DMS_COM.write_jlt(v_unit, 1, 'End '||v_unit);
             DMS_LAT.step_end(n_num_righe, '-OK');
             
            pulizia_foglicassa();
            
            /* inviamo una mail di notifica quando sono arrivati i fogli cassa giornalieri*/
            if( trim(replace(replace(l_response_text,chr(10),' '),chr(13),' ')) != trim('Found: 0 documents.')) then
                pid     := null;                                     
                pid     := testo_mail('La Trasmissione dei fogli cassa di fine giornata e'' terminata.', pid); 
                pid     := testo_mail('', pid);     
                pid     := testo_mail(trim(replace(replace(l_response_text,chr(10),' '),chr(13),' ')), pid);   
                SEND_MAIL('Trasmissione FC da 4STORE', PID, NUM_GRUPPO_DEST);
            end if;
            
            
            dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
            --ricarico la condizione di controllo del loop esterno
            select to_number(to_char(sysdate,'HH24MI'))
            into v_control
            from dual;
                       
        
      exception 
        WHEN UTL_HTTP.end_of_body THEN
           UTL_HTTP.end_response(l_http_response);
           DMS_COM.write_jlt(v_unit, 1, l_response_text); 
           dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
            --ricarico la condizione di controllo del loop esterno
           select to_number(to_char(sysdate,'HH24MI'))
           into v_control
           from dual; 
        WHEN OTHERS then
          DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
          --mail di notifica errori
          pid     := null;                                     
          pid     := testo_mail('La Trasmissione dei fogli cassa di fine giornata e'' andata in eccezione.', pid); 
          pid     := testo_mail('', pid);     
          pid     := testo_mail('Generata eccezione ' || SQLCODE || ' ' || SQLERRM, pid);   
          SEND_MAIL('Errore Trasmissione FC da 4STORE', PID, NUM_GRUPPO_DEST);
          --ripresa del loop
          dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
            --ricarico la condizione di controllo del loop esterno
          select to_number(to_char(sysdate,'HH24MI'))
          into v_control
          from dual; 
      end;
   end loop;
	 --FINE LOOP DI CONTROLLO
	 
   DMS_COM.write_jlt(v_unit,1,'Fine Controllo loop '||v_unit);


  EXCEPTION
    WHEN UTL_HTTP.end_of_body THEN
      UTL_HTTP.end_response(l_http_response);
      DMS_COM.write_jlt(v_unit, 1, l_response_text);
    WHEN OTHERS THEN  
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DCG_TRIGGER_FOGLICASSA;
  /******************************************************************************
     NAME:       DCG_TRIGGER_FATCLI
     PURPOSE:    Procedura che attiva i trigger dei servizi REST(GET) per le FATCLI

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena Attivazione periodica del trigger http per il servizio GET delle fatture clienti
  *******************************************************************************/
  PROCEDURE DCG_TRIGGER_FATCLI(p_job     IN VARCHAR2,
                               p_period IN VARCHAR2,
                               p3       IN VARCHAR2 DEFAULT NULL,
                               p4       IN VARCHAR2 DEFAULT NULL,
                               p5       IN VARCHAR2 DEFAULT NULL,
                               p6       IN VARCHAR2 DEFAULT NULL,
                               p7       IN VARCHAR2 DEFAULT NULL,
                               p8       IN VARCHAR2 DEFAULT NULL,
                               p9       IN VARCHAR2 DEFAULT NULL,
                               p10      IN VARCHAR2 DEFAULT NULL,
                               p11      IN VARCHAR2 DEFAULT NULL) IS

    k_error_marker CONSTANT CHAR(22)     := '[*** !!! ***]  [ERROR]';
    v_unit                  VARCHAR2(30) := 'DCG_TRIGGER_FATCLI';
    NUM_GRUPPO_DEST         VARCHAR2(10) := '1001';   --mailing list delle notifiche 4STORE
    PID                     NUMBER;
    v_testo                 VARCHAR2(200);
    n_num_righe             INTEGER;
    
    l_http_request          UTL_HTTP.req;
    l_http_response         UTL_HTTP.resp;
    l_response_text         VARCHAR2(32000);
    l_url                   varchar2(1000):='http://172.24.28.139:3003/fatcli/';
    l_method                varchar2(100) :='GET';
    l_protocol              varchar2(100) :='HTTP/1.1';
    
    v_control               INTEGER;
    v_count                 INTEGER          :=0;
		v_start                 INTEGER          :=0;
    v_processato            VARCHAR2(1)      :='N';
    v_loop                  boolean          :=true;
    
    crlf   CONSTANT VARCHAR2(2) := CHR(13) || CHR(10);
    cr     CONSTANT VARCHAR2(2) := CHR(13);
    lf     CONSTANT VARCHAR2(2) := CHR(10);
    
  BEGIN
   DMS_COM.write_jlt(v_unit,1,'Inizio loop '||v_unit);
    select to_number(to_char(sysdate,'HH24'))
    into v_control
    from dual;
   --ciclo continuo dall' ora di inizio job all' ora v_stop 
   while ((v_control < v_stop) and (v_loop)) loop
      begin   
         --se v_test:=true interrompiamo il ciclo
         if(v_test) then
           v_loop:=not v_loop;
         end if;
       
         DMS_LAT.step_start('Start '||v_unit, 'IN', 'Start '||v_unit);
          DMS_COM.write_jlt(v_unit, 1, 'Start '||v_unit);

          -- preparing request
          l_http_request := UTL_HTTP.begin_request(l_url,
                                                   l_method,
                                                   l_protocol);

          l_http_response := UTL_HTTP.get_response(l_http_request);
          
          UTL_HTTP.read_text(l_http_response, l_response_text);
          
          --DBMS_OUTPUT.put_line(l_response_text);
          DMS_COM.write_jlt(v_unit, 1, replace(replace(l_response_text,chr(10),' '),chr(13),' ') );
          
          UTL_HTTP.end_response(l_http_response);

          DMS_COM.write_jlt(v_unit, 1, 'End '||v_unit);
         DMS_LAT.step_end(n_num_righe, '-OK');
         
        pulizia_fatcli();
         
        dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
        --ricarico la condizione di controllo del loop esterno
        select to_number(to_char(sysdate,'HH24'))
        into v_control
        from dual;                  
      exception 
        WHEN UTL_HTTP.end_of_body THEN
           UTL_HTTP.end_response(l_http_response);
           DMS_COM.write_jlt(v_unit, 1, l_response_text); 
           dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
            --ricarico la condizione di controllo del loop esterno
           select to_number(to_char(sysdate,'HH24'))
           into v_control
           from dual; 
        WHEN OTHERS then
          DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
          --mail di notifica errori
          pid     := null;                                     
          pid     := testo_mail('La Trasmissione delle fatture cliente e'' andata in eccezione.', pid); 
          pid     := testo_mail('', pid);     
          pid     := testo_mail('Generata eccezione ' || SQLCODE || ' ' || SQLERRM, pid);   
          SEND_MAIL('Errore Trasmissione FATCLI da 4STORE', PID, NUM_GRUPPO_DEST);
          --ripresa del loop
          dbms_lock.sleep(v_timeout); --timeout di <600> secondi (10 minuti)
            --ricarico la condizione di controllo del loop esterno
          select to_number(to_char(sysdate,'HH24'))
          into v_control
          from dual;  
      end;
   end loop;
	 --FINE LOOP DI CONTROLLO
	 
   DMS_COM.write_jlt(v_unit,1,'Fine Controllo loop '||v_unit);
  
  EXCEPTION
    WHEN UTL_HTTP.end_of_body THEN
      UTL_HTTP.end_response(l_http_response);
      DMS_COM.write_jlt(v_unit, 1, l_response_text);
    WHEN OTHERS THEN  
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DCG_TRIGGER_FATCLI;
  /******************************************************************************
     NAME:       pulizia_scontrini
     PURPOSE:    Procedura che EFFETTUA LA PULIZIA DI RETENTION

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena 
  *******************************************************************************/
  PROCEDURE pulizia_scontrini IS
    k_error_marker CONSTANT CHAR(22)     := '[*** !!! ***]  [ERROR]';
    v_unit                  VARCHAR2(30) := 'pulizia_scontrini';
  BEGIN     
    DMS_COM.write_jlt(v_unit, 1,'Inizio retention json_scn_*');
    delete gefa.json_scn_testata                where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_art_virtuali           where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_art_virtuali_jret      where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_autorizz_sv            where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_dett_card              where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_ean_non_cassa          where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_fp                     where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_fp_jret                where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_iva                    where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_promo                  where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_promo_jret             where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_righe                  where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_righe_jret             where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_testata_card           where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_testata_card_jret      where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    delete gefa.json_scn_testata_jret           where dt_inserimento<sysdate-v_finestra_retention_gg_scn and upper(flg_elaborazione)='F';
    commit;
    DMS_COM.write_jlt(v_unit, 1,'End retention json_scn_*');
  EXCEPTION WHEN OTHERS THEN  
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;    
  END pulizia_scontrini;
  /******************************************************************************
     NAME:       pulizia_foglicassa
     PURPOSE:    Procedura che EFFETTUA LA PULIZIA DI RETENTION

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena 
  *******************************************************************************/  
  PROCEDURE pulizia_foglicassa IS
      k_error_marker CONSTANT CHAR(22)     := '[*** !!! ***]  [ERROR]';
    v_unit                  VARCHAR2(30) := 'pulizia_foglicassa';
  BEGIN     
    DMS_COM.write_jlt(v_unit, 1,'Inizio retention json_fc_*');
    delete gefa.json_fc_dett_eod                where dt_inserimento<sysdate-v_finestra_retention_gg_fc and upper(flg_elaborazione)='F';
    delete gefa.json_fc_dett_eod_jlog           where dt_inserimento<sysdate-v_finestra_retention_gg_fc and upper(flg_elaborazione)='F';
    delete gefa.json_fc_dett_eod_jret           where dt_inserimento<sysdate-v_finestra_retention_gg_fc and upper(flg_elaborazione)='F';
    delete gefa.json_fc_testata                 where dt_inserimento<sysdate-v_finestra_retention_gg_fc and upper(flg_elaborazione)='F';
    delete gefa.json_fc_testata_eod             where dt_inserimento<sysdate-v_finestra_retention_gg_fc and upper(flg_elaborazione)='F';
    delete gefa.json_fc_testata_eod_jlog        where dt_inserimento<sysdate-v_finestra_retention_gg_fc and upper(flg_elaborazione)='F';
    delete gefa.json_fc_testata_eod_jret        where dt_inserimento<sysdate-v_finestra_retention_gg_fc and upper(flg_elaborazione)='F';
    delete gefa.json_fc_testata_jret            where dt_inserimento<sysdate-v_finestra_retention_gg_fc and upper(flg_elaborazione)='F';
    commit;
    DMS_COM.write_jlt(v_unit, 1,'End retention json_fc_*');
  EXCEPTION WHEN OTHERS THEN  
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;   
  END pulizia_foglicassa;
  /******************************************************************************
     NAME:       pulizia_fatcli
     PURPOSE:    Procedura che EFFETTUA LA PULIZIA DI RETENTION

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        30/09/2019  Massimiliano Modena 
  *******************************************************************************/   
  PROCEDURE pulizia_fatcli IS
      k_error_marker CONSTANT CHAR(22)     := '[*** !!! ***]  [ERROR]';
    v_unit                  VARCHAR2(30) := 'pulizia_fatcli';
  BEGIN     
    DMS_COM.write_jlt(v_unit, 1,'Inizio retention json_fatcli_*');
    delete gefa.json_fatcli_testata             where dt_inserimento<sysdate-v_finestra_retention_gg_fatcli and upper(flg_elaborazione)='F';
    delete gefa.json_fatcli_invoice             where dt_inserimento<sysdate-v_finestra_retention_gg_fatcli and upper(flg_elaborazione)='F';
    delete gefa.json_fatcli_cliente             where dt_inserimento<sysdate-v_finestra_retention_gg_fatcli and upper(flg_elaborazione)='F';
    delete gefa.json_fatcli_destinatario        where dt_inserimento<sysdate-v_finestra_retention_gg_fatcli and upper(flg_elaborazione)='F';
    delete gefa.json_fatcli_articoli            where dt_inserimento<sysdate-v_finestra_retention_gg_fatcli and upper(flg_elaborazione)='F';
    delete gefa.json_fatcli_iva                 where dt_inserimento<sysdate-v_finestra_retention_gg_fatcli and upper(flg_elaborazione)='F';
    commit;
    DMS_COM.write_jlt(v_unit, 1,'End retention json_fatcli_*');
  EXCEPTION WHEN OTHERS THEN  
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END pulizia_fatcli;
  
  
END DCG_PK_4STORE;

/
