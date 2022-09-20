CREATE OR REPLACE PACKAGE DWH_PROC AS
  /******************************************************************************
     NAME:       DCG_PK_ORE_LAV
     PURPOSE:    Carica Ora lavorative

     REVISIONS:
     Ver        Date        Author                Description
     ---------  ----------  -------------------   --------------------------------
     1.0        09/01/2008  Massimiliano Modena - Softquattro
     
     2.0        11/07/2016  Fpisano             - Incassi amazon
     3.0        07/10/2019  N Delle Donne       - progetto 4s
      --aggiunta procedura DWH_PROC_LOAD_SCN_4S
      --modificate procedure\funzioni
         DWH_PROC_LOAD_SCN (segregazione, solo pdv non 4s)
         ConsolidaDwStatoScn (aggiunto settare a F scontini 4s trattati,
                              aggiunta mail di wrning PDV 4s che non ahnno inviato
                              scontini con data ieri)
  ******************************************************************************/
  v_sysdate       DATE;
  v_cutoff        INTEGER:=100;
  /* funzioni SCN */
  function all_env                    RETURN DMS_R3_COM_CFG_ENVIRONMENT%ROWTYPE;
  function TempRicalcolaBollini       RETURN varchar2;
  function AttribuisciPromo           RETURN varchar2;
  function AggiornaCodFisc            RETURN varchar2;
  function VerificaDept               RETURN varchar2;
  function CompattaRigheScn           RETURN varchar2;
  function AggiornaArticoliGiorno     RETURN varchar2;
  function ScaricaTestata             RETURN varchar2;
  function ScaricaFormePagamento      RETURN varchar2;
  function ScaricaRigheEAnomalie      RETURN varchar2;
  function ScaricaArtSettCorrente     RETURN varchar2;
  function ConsolidaDwStatoScn        RETURN varchar2;
  function ConsolidaDwStatoRep        RETURN varchar2;
  /* funzioni REP */
  function LoadRepClass(v_data DATE)  RETURN varchar2;
  function RicavaDatiRep(v_data DATE) RETURN varchar2;
  function GestisciRep(v_data DATE)   RETURN varchar2;
  function ScaricaRep                 RETURN varchar2;
  /* procedure varie */
  PROCEDURE DWH_PROC_LOAD_SCN(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE DWH_PROC_LOAD_SCN_AMZ(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);                              
  function  RicalcolaImportiCesti(v_pdv varchar2, v_date varchar2)        RETURN varchar2;
  function  MailRiepilogo4S(v_date date)        RETURN varchar2;
  PROCEDURE DWH_PROC_LOAD_SCN_4S(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE DWH_PROC_SCN_PROCESSING(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE DWH_PROC_REP_PROCESSING(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE DWH_PROC_ANOMALIE(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE WRAP_STAMPA_EAN_ANOMALIE(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE WRAP_DAILY_SH(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE WRAP_TUESDAY_SH(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE WRAP_MONTHLY1_SH(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE WRAP_MONTHLY2_SH(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE WRAP_MONTHLY3_SH(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);

  PROCEDURE DWMS_PROC_INIT(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  PROCEDURE DWMS_PROC_END(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);
  FUNCTION disableScnProva RETURN NUMBER ;
 PROCEDURE DWMS_PROC_CHECK_RESI(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL);

END DWH_PROC;
/
CREATE OR REPLACE PACKAGE BODY DWH_PROC IS

    TYPE t_articoli_unes  IS TABLE OF PROC_ARTICOLIUNES%rowtype;
    TYPE t_elenco_scn     IS TABLE OF PROC_CHIAVI_SCN%rowtype;
    TYPE t_ean            IS TABLE OF PROC_EAN%rowtype;
    TYPE t_promo          IS TABLE OF PROC_PROMO%rowtype;
    TYPE t_chiave_scn     IS TABLE OF PROC_CHIAVI_SCN%rowtype;
    TYPE t_codfiscpiva    IS TABLE OF PROC_CODFISCPIVA%rowtype;
    TYPE t_repclass       IS TABLE OF PROC_REP_CLASS%rowtype;
    TYPE t_class          IS TABLE OF PROC_CLASS%rowtype;
    TYPE t_scn            IS TABLE OF DWH_DATI_SCN_STOR%rowtype;
    TYPE t_rep            IS TABLE OF DWH_DATI_REP_STOR%rowtype;

    v_ArticoliUnes        t_articoli_unes;
    v_Elenco_Scn          t_elenco_scn;
    v_Ean                 t_ean;
    v_Promo               t_promo;
    v_ArtPromo            t_promo;
    v_Chiave_Scn          t_chiave_scn;
    v_CodFiscPiva         t_codfiscpiva;
    v_RepClass            t_repclass;
    v_Class               t_class;
    v_Scn                 t_scn;
    v_Rep                 t_rep;

  /******************************************************************************
     NAME:       VerificaDept
     PURPOSE:    Funzione che verifica il Dept con il REP_POS_ID nella anag.articolo

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        16/01/2012  Massimiliano Modena - Softquattro
     2.0        18/11/2014  Francesco Pisano    - S4 : aggiunta tabella storica e nuovi campi 
     3.0        30/07/2018  M.Modena            gestione nuovo tipo record resi
  *******************************************************************************/
  
 FUNCTION VerificaDept RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'VerificaDept';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';
    v_return_cmd            VARCHAR2(2);

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

    fHandle                 UTL_FILE.FILE_TYPE;
    vcPath                VARCHAR2(2000);
    vcName                VARCHAR2(255);
    crlf         CONSTANT VARCHAR2(2) := CHR(13) || CHR(10);
    v_command               VARCHAR2(1000);
  BEGIN
      DMS_COM.write_jlt(v_unit, 1, 'Inizio modulo verifica discrepanze');
       
      begin
        select  'elenco_discrepanze_dept.' ||
              to_char(s.dt_competenza, 'yyyymmdd')||
              '.log'
        into vcName
        from PROC_DATI_GREZZI s
        where rownum<2;
      exception  when no_data_found then
        DMS_COM.write_jlt(v_unit, 1, 'Non ci sono dati da verificare');
        RETURN v_return;
      end;
      
  --  Cancello lo storico degli elementi piu vecchi di 15 gg     
      delete PROC_DISCREPANZE_DEPT
      where dt_competenza < sysdate-15;
      DMS_COM.write_jlt(v_unit, 1, 'Svecchiamento discrepanze completato');

      DMS_COM.write_jlt(v_unit, 1, 'Nome del file vcName: '||vcName);
      

--impostiamo un reference al file di testo dove scriveremo il body della mail
  --    fHandle := utl_file.fopen('BAD_DIR', vcName, 'a'); --in write mode...
  --    DMS_COM.write_jlt(v_unit, 1, 'post fopen');

      for cur in (
                   select /*+no_index(s idx1_proc_stor) */ distinct s.dt_competenza,
                                   s.cod_pdv,
                                   e.ean_id,
                                   a.art_id,
                                   lpad(a.rep_pos_id,4,'0') as rep_pos_id,
                                   s.dept,
                                   a.stato,
                                   n.tipo_neg_id
                            from dwh_dati_scn_stor s,
                                 proc_tmp_file t,
                                 anag.ean e,
                                 anag.articolo a,
                                 anag.negozio n
                            where (tipo_record=2 or tipo_record=5)
                              and flag_art_rep='A'
                              and s.cod_ean=e.ean_id
                              and e.art_id=a.art_id
                              and s.dept!=lpad(a.rep_pos_id,4,'0')
                              and s.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                              and lpad(s.cod_pdv,4,'0') = substr(t.nome_file, 4, 4)
                              and t.caricato = 'Y'
                              and a.rep_pos_id not in ('7','9','91','97','98','16')
                              and lpad(s.cod_pdv,4,'0') = lpad(n.neg_id,4,'0')
                              order by s.cod_pdv, a.art_id, s.dt_competenza
                   )
      loop


-- creo la tabella con lo storico --max 15 giorni poi svecchia in testa 
           Insert Into Etl.Proc_Discrepanze_Dept (
                       Dt_Competenza, Cod_Pdv, Ean_Id,Art_Id, Rep_Pos_Id, Dept,Stato, Tipo_Neg_Id) 
                        Values (Cur.Dt_Competenza,
                        Cur.Cod_Pdv,
                        Cur.Ean_Id,
                        Cur.Art_Id,
                        Cur.Rep_Pos_Id,
                        Cur.Dept,Cur.Stato,
                        Cur.Tipo_Neg_Id);


      end loop;

      DMS_COM.write_jlt(v_unit, 1, 'post loop');



   COMMIT;

    DMS_COM.write_jlt(v_unit, 1, 'fine modulo verifica discrepanze'||v_return);

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END VerificaDept;
  
  /******************************************************************************
     NAME:       CompattaRigheScn
     PURPOSE:    Funzione che compatta le righe degli scontrini

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        24/11/2011  Massimiliano Modena - Softquattro
     2.0        30/07/2018  M.Modena            gestione nuovo tipo record resi
  *******************************************************************************/
  FUNCTION CompattaRigheScn RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'CompattaRigheScn';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN
            ---compattamento dello scontrino --righe
    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Chiave_Scn');

    v_Chiave_Scn.DELETE; --svuotamento della collection

    DMS_COM.write_jlt(v_unit, 1, 'ripopolamento della collection v_Chiave_Scn');

      SELECT * BULK COLLECT
        INTO v_Chiave_Scn
        from (select distinct dt_competenza,
                              cod_pdv,
                              cod_cassa,
                              cod_transazione,
                              scn_key
                from (select count(*) conteggio,
                             dt_competenza,
                             cod_pdv,
                             cod_cassa,
                             cod_transazione,
                             tipo_record,
                             cod_art_rep,
                             tipo_riga,
                             d.scn_key
                        from dwh_dati_scn_stor d, proc_tmp_file t
                       where (tipo_record = 2 or tipo_record = 5)
                         and cod_art_rep is not null
                         and length(cod_art_rep) >= 5
                         and d.dt_competenza =
                             to_date(substr(t.nome_file, 9), 'rrmmdd')
                         and lpad(d.cod_pdv,4,'0') = substr(t.nome_file, 4, 4)
                         and t.caricato = 'Y'
                       group by dt_competenza,
                                cod_pdv,
                                cod_cassa,
                                cod_transazione,
                                tipo_record,
                                cod_art_rep,
                                tipo_riga,
                                d.scn_key
                      having count(*) > 1
                       order by dt_competenza,
                                cod_pdv,
                                cod_cassa,
                                cod_transazione,
                                tipo_record));

      v_cicli:=0;
      v_count:=0;
      v_found:=0;
      v_count_exc:=0;

      for idx_key in 1 .. v_Chiave_Scn.COUNT
       loop
         v_cicli:=v_cicli+1;
       --segno da cancellare i record che devo ricoprire
       update DWH_DATI_SCN_STOR d
       set d.dt_caricamento=null
        where d.scn_key=v_Chiave_Scn(idx_key).scn_key
          and (tipo_record = 2 or tipo_record = 5);

       v_found:=v_found+SQL%rowcount;

       --inserisco i nuovi record raggruppati
       insert  into DWH_DATI_SCN_STOR
          select tipo_record,cod_pdv,cod_transazione,cod_cassa,
                 dt_competenza,ora_inizio,ora_fine,cod_cliente,
                 flg_cliente,cd_remoto,cd_fisc_piva,classe,
                 num_articoli,tot_netto,sconto_transaz,sconto_articoli,
                 sconto_transaz_fidelity,sconto_articoli_fidelity,
                 bollini_scontrino,tipo_pagam,valore_netto,
                 min(cod_ean),
                 cod_art_rep,tipo_riga,tipo_sconto,flag_art_rep,dept,
                 sum(quantita),
                 sum(peso_per_1000),
                 sum(valore_netto_riga),
                 sum(sconto_riga),
                 sum(sconto_fidelity),
                 sum(sconto_trx_ventilato),
                 sum(bollini_riga),
                 scn_key,
                 sysdate,
                 promo_id,
                 num_sco_cli,
                 tipo_promo_id,
                 scontrino_prog,
                 reso_num_cassa,
                 reso_num_transazione,
                 reso_num_azzeramento,
                 reso_dt_transazione,
                 reso_ora_transazione,
                 null flg_introduzione_ora
            from dwh_dati_scn_stor
           where (tipo_record = 2 or tipo_record = 5)
             and scn_key = v_Chiave_Scn(idx_key).scn_key
           group by tipo_record,cod_pdv,cod_transazione,cod_cassa,
                 dt_competenza,ora_inizio,ora_fine,cod_cliente,
                 flg_cliente,cd_remoto,cd_fisc_piva,classe,
                 num_articoli,tot_netto,sconto_transaz,sconto_articoli,
                 sconto_transaz_fidelity,sconto_articoli_fidelity,
                 bollini_scontrino,tipo_pagam,valore_netto,
                 cod_art_rep,tipo_riga,tipo_sconto,flag_art_rep,dept,
                 scn_key,promo_id,num_sco_cli,tipo_promo_id,scontrino_prog,
                 reso_num_cassa,reso_num_transazione,reso_num_azzeramento,
                 reso_dt_transazione,reso_ora_transazione;
         v_count:=v_count+SQL%rowcount;
         --cancello i vecchi record
         delete DWH_DATI_SCN_STOR d
         where d.scn_key=v_Chiave_Scn(idx_key).scn_key
          and (tipo_record = 2 or tipo_record = 5)
          and dt_caricamento is null;

         v_count_exc:=v_count_exc+SQL%rowcount;

      end loop;

      dbms_output.put_line(' ');
      dbms_output.put_line('#cicli di raggruppamento righe COD_EAN: '||to_char(v_cicli));
      dbms_output.put_line('#marcati null: '||to_char(v_found));
      dbms_output.put_line('#reinseriti: '||to_char(v_count));
      dbms_output.put_line('#cancellazioni: '||to_char(v_count_exc));

      DMS_COM.write_jlt(v_unit, 1, '#cicli di raggruppamento righe COD_EAN: '||to_char(v_cicli));
      DMS_COM.write_jlt(v_unit, 1, '#marcati null: '||to_char(v_found));
      DMS_COM.write_jlt(v_unit, 1, '#reinseriti: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#cancellazioni: '||to_char(v_count_exc));


   COMMIT;
    --pulizia della collection non piu' utilizzata
    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Chiave_Scn');

    v_Chiave_Scn.DELETE;
        /************* CR 20120726_1130: Gestione di piu dept corrispondenti a stesso codice articolo prendendo il MAX di quelli disponibili */


    SELECT * BULK COLLECT
        INTO v_Chiave_Scn
        from (select distinct dt_competenza,
                              cod_pdv,
                              cod_cassa,
                              cod_transazione,
                              scn_key
                from (select count(*) conteggio,
                             dt_competenza,
                             cod_pdv,
                             cod_cassa,
                             cod_transazione,
                             tipo_record,
                             --dept,
                             cod_art_rep,
                             tipo_riga,
                             d.scn_key
                        from dwh_dati_scn_stor d, proc_tmp_file t
                       where (tipo_record = 2 or tipo_record = 5)
                         and cod_art_rep is not null
                         and length(cod_art_rep) >= 5
                         and d.dt_competenza =
                             to_date(substr(t.nome_file, 9), 'rrmmdd')
                         and lpad(d.cod_pdv,4,'0') = substr(t.nome_file, 4, 4)
                         and t.caricato = 'Y'
                       group by dt_competenza,
                                cod_pdv,
                                cod_cassa,
                                cod_transazione,
                                tipo_record,
                                --dept,
                                cod_art_rep,
                                tipo_riga,
                                d.scn_key
                      having count(*) > 1
                       order by dt_competenza,
                                cod_pdv,
                                cod_cassa,
                                cod_transazione,
                                tipo_record));

      v_cicli:=0;
      v_count:=0;
      v_found:=0;
      v_count_exc:=0;


     for idx_key in 1 .. v_Chiave_Scn.COUNT
       loop
         v_cicli:=v_cicli+1;

       --segno da cancellare i record che devo ricoprire
       update DWH_DATI_SCN_STOR d
       set d.dt_caricamento=null
        where d.scn_key=v_Chiave_Scn(idx_key).scn_key
          and (tipo_record = 2 or tipo_record = 5);

       v_found:=v_found+SQL%rowcount;

              --inserisco i nuovi record raggruppati per DEPT
       insert into DWH_DATI_SCN_STOR
          select tipo_record,cod_pdv,cod_transazione,cod_cassa,
                 dt_competenza,ora_inizio,ora_fine,cod_cliente,
                 flg_cliente,cd_remoto,cd_fisc_piva,classe,
                 num_articoli,tot_netto,sconto_transaz,sconto_articoli,
                 sconto_transaz_fidelity,sconto_articoli_fidelity,
                 bollini_scontrino,tipo_pagam,valore_netto,
                 min(cod_ean),
                 cod_art_rep,tipo_riga,tipo_sconto,flag_art_rep,
                 max(dept),
                 sum(quantita),
                 sum(peso_per_1000),
                 sum(valore_netto_riga),
                 sum(sconto_riga),
                 sum(sconto_fidelity),
                 sum(sconto_trx_ventilato),
                 sum(bollini_riga),
                 scn_key,
                 sysdate,
                 promo_id,
                 num_sco_cli,
                 tipo_promo_id,
                 scontrino_prog,
                 reso_num_cassa,
                 reso_num_transazione,
                 reso_num_azzeramento,
                 reso_dt_transazione,
                 reso_ora_transazione,
                 null flg_introduzione_ora
            from dwh_dati_scn_stor
           where (tipo_record = 2 or tipo_record = 5)
             and scn_key = v_Chiave_Scn(idx_key).scn_key
           group by tipo_record,cod_pdv,cod_transazione,cod_cassa,
                 dt_competenza,ora_inizio,ora_fine,cod_cliente,
                 flg_cliente,cd_remoto,cd_fisc_piva,classe,
                 num_articoli,tot_netto,sconto_transaz,sconto_articoli,
                 sconto_transaz_fidelity,sconto_articoli_fidelity,
                 bollini_scontrino,tipo_pagam,valore_netto,
                 cod_art_rep,tipo_riga,tipo_sconto,flag_art_rep,
                 scn_key,promo_id,num_sco_cli,tipo_promo_id,scontrino_prog,
                 reso_num_cassa,reso_num_transazione,reso_num_azzeramento,
                 reso_dt_transazione,reso_ora_transazione;

         v_count:=v_count+SQL%rowcount;

         --cancello i vecchi record
         delete DWH_DATI_SCN_STOR d
         where d.scn_key=v_Chiave_Scn(idx_key).scn_key
          and (tipo_record = 2 or tipo_record = 5)
          and dt_caricamento is null;

         v_count_exc:=v_count_exc+SQL%rowcount;

      end loop;


      dbms_output.put_line(' ');
      dbms_output.put_line('#cicli di raggruppamento righe DEPT: '||to_char(v_cicli));
      dbms_output.put_line('#marcati null: '||to_char(v_found));
      dbms_output.put_line('#reinseriti: '||to_char(v_count));
      dbms_output.put_line('#cancellazioni: '||to_char(v_count_exc));


      DMS_COM.write_jlt(v_unit, 1, '#cicli di raggruppamento righe DEPT: '||to_char(v_cicli));
      DMS_COM.write_jlt(v_unit, 1, '#marcati null DEPT: '||to_char(v_found));
      DMS_COM.write_jlt(v_unit, 1, '#reinseriti DEPT: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#cancellazioni DEPT: '||to_char(v_count_exc));


      COMMIT;
      --pulizia della collection non piu' utilizzata
      dbms_output.put_line( 'pulizia collection v_Chiave_Scn x DEPT');

    v_Chiave_Scn.DELETE;

    /** FINE CR 20120726_1130 ***********/


    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END CompattaRigheScn;

  /******************************************************************************
     NAME:       TempRicalcolaBollini
     PURPOSE:    Funzione che ricalcola i bollini premio degli scontrini

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        24/11/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  FUNCTION TempRicalcolaBollini RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'TempRicalcolaBollini';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;


  BEGIN
      dbms_output.put_line('******');
      dbms_output.put_line('Begin '||v_unit);

        --v_Elenco_Scn.DELETE; --svuotamento della collection
        --recuperiamo la lista degli scontrini da processare (che hanno qualche bollino premio)
        DMS_COM.write_jlt(v_unit, 1, 'popolamento collection');


         SELECT * BULK COLLECT
           INTO v_Elenco_Scn
           from (select distinct dt_competenza,
                                 cod_pdv,
                                 cod_cassa,
                                 cod_transazione,
                                 scn_key
                   from dwh_dati_scn_stor d, proc_tmp_file t
                  where 1 = 1
                    and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                    and lpad(d.cod_pdv,4,'0') = substr(t.nome_file, 4, 4)
                    and t.caricato = 'Y'
                    --and to_number(d.dept)=9
                    and d.dept='0009'
                    and d.bollini_riga != 0);


      v_cicli := 0;
      v_count:=0;
      v_found:=0;
      v_count_exc:=0;

     dbms_output.put_line('Scontrini da processare: '||to_char(v_Elenco_Scn.COUNT));
     DMS_COM.write_jlt(v_unit, 1, 'Scontrini da processare: '||to_char(v_Elenco_Scn.COUNT));


     ---sommiamo i punti dei bollini_premio nelle varie righe per ogni scontrino
     ---e li decurtiamo dai bollini totali dello scontrino
     for idx in 1 .. v_Elenco_Scn.COUNT
      loop
         v_cicli:=v_cicli+1;

           select sum(ABS(nvl(bollini_riga, 0)))
             into v_bollini_premio
             from dwh_dati_scn_stor d
            where 1 = 1
              and tipo_record = 2 --righe scontrino
              --and to_number(d.dept)=9
                    and d.dept='0009'
              and d.scn_key = v_Elenco_Scn(idx).scn_key;

           v_found:=v_found+SQL%rowcount;

           update dwh_dati_scn_stor t
              set t.bollini_scontrino=t.bollini_scontrino-v_bollini_premio
            where 1 = 1
              and tipo_record = 0 --testata scontrino
              --and to_number(d.dept)=9
                    and t.dept='0009'
              and t.scn_key = v_Elenco_Scn(idx).scn_key;

           v_count:=v_count+SQL%rowcount;

      end loop;

      dbms_output.put_line('#cicli di raggruppamento bollini: '||to_char(v_cicli));
      dbms_output.put_line('#rec bollini: '||to_char(v_found));
      dbms_output.put_line('#ricalcoli bollini scontrini: '||to_char(v_count));

      DMS_COM.write_jlt(v_unit, 1, '#cicli di raggruppamento bollini: '||to_char(v_cicli));
      DMS_COM.write_jlt(v_unit, 1, '#rec bollini: '||to_char(v_found));
      DMS_COM.write_jlt(v_unit, 1, '#ricalcoli bollini scontrini: '||to_char(v_count));

   COMMIT;
    --pulizia della collection non piu' utilizzata
    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Elenco_Scn');

    v_Elenco_Scn.DELETE;

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';

  END TempRicalcolaBollini;

  /******************************************************************************
     NAME:       AttribuisciPromo
     PURPOSE:    Funzione che Attribuisce i corretti codici Promo agli articoli
                 nelle righe scontrino

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        24/11/2011  Massimiliano Modena - Softquattro
     2.0        30/07/2018  M.Modena            gestione nuovo tipo record resi
  *******************************************************************************/
  FUNCTION AttribuisciPromo RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'AttribuisciPromo';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN
        dbms_output.put_line('******');
        dbms_output.put_line('Begin '||v_unit);
        --v_ArtPromo.DELETE; --svuotamento della collection
        --recuperiamo la lista delle righe articoli che hanno promo_id non valorizzato
        DMS_COM.write_jlt(v_unit, 1, 'popolamento collection');

         SELECT * BULK COLLECT
           INTO v_ArtPromo
           from (select distinct d.cod_art_rep,null, d.cod_pdv, d.dt_competenza
                   from dwh_dati_scn_stor d, proc_tmp_file t
                  where 1 = 1
                    and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                    and lpad(d.cod_pdv,4,'0') = substr(t.nome_file, 4, 4)
                    and t.caricato = 'Y'
                    and (d.tipo_record=2 or d.tipo_record=5)
                    and d.flag_art_rep='A'
                    and d.promo_id is null);

      dbms_output.put_line('#da processare: '||to_char(v_ArtPromo.COUNT));
      DMS_COM.write_jlt(v_unit, 1, '#da processare: '||to_char(v_ArtPromo.COUNT));


      v_cicli := 0;
      v_count:=0;
      v_found:=0;
      v_count_exc:=0;

      for idx in 1 .. v_ArtPromo.COUNT
       loop

           v_cicli:=v_cicli+1;
           begin
             select max(p.promo_id)
               into v_promo_id
               from proc_promo p
              where art_promo = v_ArtPromo(idx).art_promo
                and cod_pdv = v_ArtPromo(idx).cod_pdv
                and dt_competenza = v_ArtPromo(idx).dt_competenza;

               v_found:=v_found+SQL%rowcount;


            UPDATE /*+ index (d idx1_proc_stor) */ DWH_DATI_SCN_STOR d
            SET d.promo_id=v_promo_id
            where 1=1
              and (d.tipo_record=2 or d.tipo_record=5)
              and d.flag_art_rep='A'
              and d.dt_competenza=v_ArtPromo(idx).dt_competenza
              and d.cod_pdv=v_ArtPromo(idx).cod_pdv
              and d.cod_art_rep=v_ArtPromo(idx).art_promo;

           v_count:=v_count+SQL%rowcount;

           exception when NO_DATA_FOUND then
             v_exceptions:=v_exceptions+1;
             --nessuna azione da compiere
           end;


       end loop;


      dbms_output.put_line('#cicli: '||to_char(v_cicli));
      dbms_output.put_line('#trovati: '||to_char(v_found));
      dbms_output.put_line('#modificati: '||to_char(v_count));
      dbms_output.put_line('#eccezioni trovate: '||to_char(v_exceptions));

      DMS_COM.write_jlt(v_unit, 1, '#cicli: '||to_char(v_cicli));
      DMS_COM.write_jlt(v_unit, 1, '#trovati: '||to_char(v_found));
      DMS_COM.write_jlt(v_unit, 1, '#modificati: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#eccezioni trovate: '||to_char(v_exceptions));

    COMMIT;
    --pulizia della collection non piu' utilizzata
    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_ArtPromo');

    v_ArtPromo.DELETE;

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END AttribuisciPromo;

  /******************************************************************************
     NAME:       AggiornaCodFisc
     PURPOSE:    Funzione che aggiorna l'anagrafica cod_fisc_piva

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        24/11/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  FUNCTION AggiornaCodFisc RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'AggiornaCodFisc';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN
        DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Scn');

        v_Scn.DELETE; --pulizia della collection

        dbms_output.put_line('******');
        dbms_output.put_line('Begin '||v_unit);

        DMS_COM.write_jlt(v_unit, 1, 'ripopolamento collection');

        SELECT * BULK COLLECT
          INTO v_Scn
          from (select distinct d.*
                  from dwh_dati_scn_stor d, proc_tmp_file t
                 where 1 = 1
                   and d.tipo_record = 0
                   and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                   and lpad(d.cod_pdv,4,'0') = substr(t.nome_file, 4, 4)
                   and t.caricato = 'Y'
                   and length(d.cd_fisc_piva) > 10
                   order by cd_fisc_piva);

        dbms_output.put_line('#da processare: '||to_char(v_Scn.COUNT));
        DMS_COM.write_jlt(v_unit, 1, '#da processare: '||to_char(v_Scn.COUNT));

        v_cicli := 0;
        v_count:=0;
        v_count2:=0;
        v_found:=0;
        v_count_exc:=0;

        for idx in 1 .. v_Scn.COUNT
        loop

           v_cicli:=v_cicli+1;

             UPDATE ANAG.CLIENTE
             SET COD_FISC_PIVA = v_Scn(idx).cd_fisc_piva
             WHERE CLI_ID = v_Scn(idx).cod_cliente;

             if(SQL%rowcount != 0) then
                v_count:=v_count+SQL%rowcount;
             else
               v_exceptions:=v_exceptions+1;
             end if;


           if(NVL(length(v_Scn(idx).cod_cliente),0)<=2) then

             v_count_exc:=v_count_exc+1;

             update /*+ index (d idx5_proc_stor) */ dwh_dati_scn_stor d
                set d.cod_cliente=v_Scn(idx).cd_fisc_piva
              where d.tipo_record=0
                and d.cd_fisc_piva=v_Scn(idx).cd_fisc_piva
                and d.scn_key=v_Scn(idx).scn_key;

             v_count2:=v_count2+SQL%rowcount;
           end if;


        end loop;


        dbms_output.put_line('#cicli: '||to_char(v_cicli));
        dbms_output.put_line('#clienti modificati: '||to_char(v_count));
        dbms_output.put_line('clienti non modificati: '||to_char(v_exceptions));
        dbms_output.put_line('#cod_cliente ripopolati: '||to_char(v_count_exc));
        dbms_output.put_line('#rec ripopolati: '||to_char(v_count2));

        DMS_COM.write_jlt(v_unit, 1, '#cicli: '||to_char(v_cicli));
        DMS_COM.write_jlt(v_unit, 1, '#clienti modificati: '||to_char(v_count));
        DMS_COM.write_jlt(v_unit, 1, 'clienti non modificati: '||to_char(v_exceptions));
        DMS_COM.write_jlt(v_unit, 1, '#cod_cliente ripopolati: '||to_char(v_count_exc));
        DMS_COM.write_jlt(v_unit, 1, '#rec ripopolati: '||to_char(v_count2));

    COMMIT;

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END AggiornaCodFisc;


  /******************************************************************************
     NAME:       AggiornaArticoliGiorno
     PURPOSE:    Funzione che aggiorna la struttura ArticoliGiorno

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        02/12/2011  Massimiliano Modena - Softquattro
     2.0        30/07/2018  M.Modena            gestione nuovo tipo record resi
  *******************************************************************************/
  FUNCTION AggiornaArticoliGiorno RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'AggiornaArticoliGiorno';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN

    --v_ArticoliUnes.DELETE; --pulizia della collection

        dbms_output.put_line('******');
        dbms_output.put_line('Begin '||v_unit);

    DMS_COM.write_jlt(v_unit, 1, 'truncate table proc_articoliunes');

    execute immediate 'truncate table proc_articoliunes';

    DMS_COM.write_jlt(v_unit, 1, 'popolamento collection');

 SELECT * BULK COLLECT
   INTO v_ArticoliUnes
   from (select cod_pdv,
                data,
                ART_ID,
                PROMO_ID,
                TIPO_DOC,
                FL_MARCA,
                sum(FS_QTA),
                sum(FS_PESO),
                sum(FS_IMP),
                sum(FS_SCONTO),
                sum(NFS_QTA),
                sum(NFS_PESO),
                sum(NFS_IMP),
                sum(NFS_SCONTO),
                sum(BOLLINI),
                sum(FS_SCONTO_TRX),
                sum(NSF_SCONTO_TRX),
                COD_EAN
           from (SELECT substr(t.nome_file, 4, 4) as cod_pdv,
                        to_date(substr(t.nome_file, 9), 'rrmmdd') as data,
                        A.ART_ID as ART_ID,
                        d.promo_id as PROMO_ID,
                        case when d.tipo_record = 2 then 'V' 
                             when d.tipo_record = 5 then 'R' end TIPO_DOC,
                        0 as FL_MARCA,
                        case when flg_cliente = 1 then sum(d.quantita) end as FS_QTA,
                        case when flg_cliente = 1 then sum(d.peso_per_1000) end as FS_PESO,
                        case when flg_cliente = 1 then sum(d.valore_netto_riga) end as FS_IMP,
                        case when flg_cliente = 1 then sum(d.sconto_riga + d.sconto_fidelity) end as FS_SCONTO,
                        case when flg_cliente = 0 and d.tipo_record = 2 then sum(d.quantita) 
                             when flg_cliente = 0 and d.tipo_record = 5 then -1 * sum(d.quantita) end as NFS_QTA,
                        case when flg_cliente = 0 and d.tipo_record = 2 then sum(d.peso_per_1000) 
                             when flg_cliente = 0 and d.tipo_record = 5 then -1 * sum(d.peso_per_1000) end as NFS_PESO,
                        case when flg_cliente = 0 and d.tipo_record = 2 then sum(d.valore_netto_riga) 
                             when flg_cliente = 0 and d.tipo_record = 5 then -1 * sum(d.valore_netto_riga) end as NFS_IMP,
                        case when flg_cliente = 0 and d.tipo_record = 2 then sum(d.sconto_riga + d.sconto_fidelity) 
                             when flg_cliente = 0 and d.tipo_record = 5 then -1 * sum(d.sconto_riga + d.sconto_fidelity) end as NFS_SCONTO,
                        sum(d.bollini_riga) as BOLLINI,
                        case when flg_cliente = 1 then sum(d.sconto_trx_ventilato) end as FS_SCONTO_TRX,
                        case when flg_cliente = 0 then sum(d.sconto_trx_ventilato) end as NSF_SCONTO_TRX,
                        d.COD_EAN as COD_EAN
                   FROM anag.ARTICOLO     A,
                        anag.MARCA        M,
                        proc_tmp_file     t,
                        DWH_DATI_SCN_STOR d
                  WHERE A.MARCA_ID = M.MARCA_ID(+)
                    and lpad(d.cod_pdv,4,'0')         = substr(t.nome_file, 4, 4)
                    and d.dt_competenza   = to_date(substr(t.nome_file, 9), 'rrmmdd')
                    and (d.tipo_record = 2 or d.tipo_record =5)
                    and d.cod_art_rep = A.ART_ID
                    and t.caricato='Y'
                  group by d.cod_pdv,
                           d.dt_competenza,
                           d.tipo_record,
                           d.cod_art_rep,
                           substr(t.nome_file, 4, 4),
                           to_date(substr(t.nome_file, 9), 'rrmmdd'),
                           A.ART_ID,
                           d.promo_id,
                           flg_cliente,
                           d.COD_EAN)
          group by cod_pdv, data, ART_ID, PROMO_ID, TIPO_DOC, FL_MARCA, COD_EAN
          order by cod_pdv, art_id, COD_EAN);

        dbms_output.put_line('#da processare: '||to_char(v_ArticoliUnes.COUNT));
        DMS_COM.write_jlt(v_unit, 1, '#da processare: '||to_char(v_ArticoliUnes.COUNT));

      v_cicli:=0;
      v_count:=0;
      v_found:=0;
      v_count_exc:=0;

      FORALL idx IN 1 .. v_ArticoliUnes.COUNT
      INSERT INTO proc_articoliunes
      VALUES
      v_ArticoliUnes(idx);

      v_count:=v_count+SQL%rowcount;

    COMMIT;

      --dbms_output.put_line(' ');

      v_cicli:=v_ArticoliUnes.COUNT;

      dbms_output.put_line('#v_ArticoliUnes.COUNT: '||to_char(v_cicli));
      dbms_output.put_line('#rec inseriti nella proc_articoliunes: '||to_char(v_count));

      DMS_COM.write_jlt(v_unit, 1, '#v_ArticoliUnes.COUNT: '||to_char(v_cicli));
      DMS_COM.write_jlt(v_unit, 1, '#rec inseriti nella proc_articoliunes: '||to_char(v_count));

      --pulizia della collection non piu' utilizzata
      DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_articoliUnes');

      v_articoliUnes.DELETE;
      --a questo punto occorre sbiancare i codici degli articoli presenti nelle righe
      --degli scontrini ma non riscontrati nell'anagrafica anag.ARTICOLO


       v_cicli:=0;
       v_count:=0;
       v_found:=0;
       v_count_exc:=0;

      DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Scn');

      v_Scn.DELETE;

      DMS_COM.write_jlt(v_unit, 1, 'debug: dopo la v_Scn.DELETE');

      --versione alternativa senza i bulk load

      for cur in (SELECT d.*
              from DWH_DATI_SCN_STOR d, proc_tmp_file t
                       where 1 = 1
                         and d.tipo_record = 2 --solo le righe
                         and d.flag_art_rep = 'A'
                         and lpad(d.cod_pdv,4,'0')                = substr(t.nome_file, 4, 4)
                         and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                         and t.caricato = 'Y') loop


             v_cicli:=v_cicli+1;

             select count(*) into v_count
             from anag.articolo a
             where a.art_id=cur.cod_art_rep;

             if(v_count=0) then

                update /*+ index (d idx5_proc_stor) */ DWH_DATI_SCN_STOR t
                set t.cod_art_rep=null
                where t.scn_key=cur.scn_key
                  and t.tipo_record=2
                  and t.flag_art_rep='A'
                  and t.cod_art_rep=cur.cod_art_rep;

                v_found:=v_found+SQL%rowcount;

             end if;

      end loop;

      dbms_output.put_line('#articoli da ripulire: '||to_char(v_cicli));
      dbms_output.put_line('#rec articoli ripuliti: '||to_char(v_found));

      DMS_COM.write_jlt(v_unit, 1, '#articoli da ripulire: '||to_char(v_cicli));
      DMS_COM.write_jlt(v_unit, 1, '#rec articoli ripuliti: '||to_char(v_found));


   COMMIT;

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END AggiornaArticoliGiorno;

  /******************************************************************************
     NAME:       ScaricaTestata
     PURPOSE:    Funzione che scarica le testate in tabella (PROC e DIAG)

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        05/12/2011  Massimiliano Modena - Softquattro
     2.0        31/07/2018  M.Modena            gestione nuovo tipo record resi
  *******************************************************************************/
  FUNCTION ScaricaTestata RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'ScaricaTestata';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3                NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;
    v_seq                 number;
  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);
  ---bisogna inserire i progressivi scontrino nella tabella base
   DMS_COM.write_jlt(v_unit, 1, 'inserimento progressivi scontrino');
  dbms_output.put_line('inserimento progressivi scontrino');

  for cur in (select d.scn_key
              from DWH_DATI_SCN_STOR d,proc_tmp_file t
                where 1=1
                and d.tipo_record in (0,3)
                and lpad(d.cod_pdv,4,'0')=substr(t.nome_file, 4, 4)
                and d.dt_competenza=to_date(substr(t.nome_file, 9), 'rrmmdd')
                and t.caricato='Y') loop

    select proc.scontrino_prog.nextval
    into v_seq
    from dual;

    update DWH_DATI_SCN_STOR d
    set d.scontrino_prog=v_seq
    where d.scn_key=cur.scn_key;

  end loop;
      dbms_output.put_line('update count '||SQL%rowcount);  
  commit;

  --ripulisco DIAG.TESTATA
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco DIAG.TESTATA');
  dbms_output.put_line('ripulisco DIAG.TESTATA');

  delete DIAG.TESTATA t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');

  dbms_output.put_line('ripulita DIAG.TESTATA '||SQL%rowcount);
  --ripulisco PROC.TESTATA
   DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.TESTATA');
  dbms_output.put_line('ripulisco PROC.TESTATA');

  delete PROC.TESTATA t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');

  DMS_COM.write_jlt(v_unit, 1,'ripulita PROC.TESTATA '||SQL%rowcount);

  --ripulisco PROC.RESO
   DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.RESO');
   dbms_output.put_line('ripulisco PROC.RESO');

  delete PROC.RESO t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');

  dbms_output.put_line('ripulita PROC.RESO '||SQL%rowcount);

   --carico DIAG.TESTATA (a partire da testate che hanno almeno una riga associata)
   DMS_COM.write_jlt(v_unit, 1, 'carico DIAG.TESTATA');
   dbms_output.put_line('carico DIAG.TESTATA');
   
  for cur in (select  d.scontrino_prog,
           d.cod_pdv,
           d.dt_competenza,
           d.ora_inizio,
           d.ora_fine,
           NVL(decode(d.cod_cliente,d.cd_fisc_piva,'000000000000',d.cod_cliente),'000000000000') cli_id,
           d.flg_cliente,
           d.cd_remoto,
           d.num_articoli,
           (d.tot_netto),-- / 100,
           (d.sconto_transaz_fidelity) ,--/ 100,
           (d.sconto_articoli_fidelity),-- / 100,
           (d.sconto_transaz) ,--/ 100,
           (d.sconto_articoli),-- / 100,
           d.bollini_scontrino,
           d.cod_cassa,
           d.cd_fisc_piva,
           d.classe
      from DWH_DATI_SCN_STOR d
     where 1 = 1
       and d.tipo_record = 0
       and d.scontrino_prog in
           (select  d.scontrino_prog
              from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
             where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
               and d.tipo_record in (0, 2)
               and lpad(d.cod_pdv ,4,'0')  = substr(t.nome_file, 4, 4)
               and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
               and t.caricato = 'Y'
               and f.filtro_id in ('D')
             group by d.scontrino_prog
            having count(*) > 1)) loop
            
    INSERT INTO DIAG.TESTATA
    select cur.scontrino_prog,
           cur.cod_pdv,
           cur.dt_competenza,
           cur.ora_inizio,
           case when to_number(cur.cli_id)>0 then to_char(to_number(cur.cli_id)) else cur.cli_id end,
           cur.flg_cliente,
           cur.cd_remoto,
           cur.num_articoli,
           cur.tot_netto,
           cur.sconto_transaz_fidelity,
           cur.sconto_articoli_fidelity,
           cur.sconto_transaz,
           cur.sconto_articoli,
           cur.bollini_scontrino,
           cur.cod_cassa,
           cur.cd_fisc_piva,
           cur.classe
      from dual;
    v_count := v_count +1;
  end loop;   
  
   
    --carico PROC.TESTATA (a partire da testate che hanno almeno una riga associata)
     DMS_COM.write_jlt(v_unit, 1, 'carico PROC.TESTATA');
    dbms_output.put_line('carico PROC.TESTATA');

   for cur2 in (
       select  /*+ ORDERED */  d.scontrino_prog,
               d.cod_pdv,
               d.dt_competenza,
               d.ora_inizio,
               d.ora_fine,
               NVL(decode(d.cod_cliente,d.cd_fisc_piva,'000000000000',d.cod_cliente),'000000000000') cli_id,
               d.flg_cliente,
               d.cd_remoto,
               d.num_articoli,
               (d.tot_netto) ,--/ 100,
               (d.sconto_transaz_fidelity),-- / 100,
               (d.sconto_articoli_fidelity),-- / 100,
               (d.sconto_transaz),-- / 100,
               (d.sconto_articoli),-- / 100,
               d.bollini_scontrino,
               d.cod_cassa,
               d.cd_fisc_piva,
               d.classe,
               d.cod_transazione
          from DWH_DATI_SCN_STOR d
         where 1 = 1
           and d.tipo_record = 0
           and d.scontrino_prog in
               (select  d.scontrino_prog
                  from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
                 where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
                   and d.tipo_record in (0, 2)
                   and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
                   and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                   and t.caricato = 'Y'
                   and f.filtro_id in ('P')
                 group by d.scontrino_prog
                having count(*) > 1)
   ) loop
   begin 
     INSERT  INTO PROC.TESTATA
     select cur2.scontrino_prog,
            cur2.cod_pdv,
            cur2.dt_competenza,
            cur2.ora_inizio,
            case when to_number(cur2.cli_id)>0 then to_char(to_number(cur2.cli_id)) else cur2.cli_id end ,
            cur2.flg_cliente,
            cur2.cd_remoto,
            cur2.num_articoli,
            cur2.tot_netto,
            cur2.sconto_transaz_fidelity,
            cur2.sconto_articoli_fidelity,
            cur2.sconto_transaz,
            cur2.sconto_articoli,
            cur2.bollini_scontrino,
            cur2.cod_cassa,
            cur2.cd_fisc_piva,
            cur2.classe,
            cur2.cod_transazione
     from dual;
     v_count2 := v_count2 +1;
   exception when others then 
     DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
       DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||SQLERRM);
       DMS_COM.write_jlt(v_unit, 1, cur2.scontrino_prog ||' - '||cur2.cod_pdv||' - '||cur2.dt_competenza);
      dbms_output.put_line('Generata eccezione ' || SQLCODE || ' ' ||SQLERRM);
      
       DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
   end;
   end loop;
    
    
    --carico PROC.RESO (a partire da testate di reso che hanno almeno una riga associata)
     DMS_COM.write_jlt(v_unit, 1, 'carico PROC.RESO');
    dbms_output.put_line('carico PROC.RESO');

   for cur3 in (
       select  /*+ ORDERED */  
               d.scontrino_prog,
               d.cod_pdv,
               d.dt_competenza,
               d.ora_inizio,
               d.ora_fine,
               d.num_articoli,
               d.tot_netto ,
               d.cod_cassa,
               d.reso_num_cassa,
               d.reso_num_transazione,
               d.reso_num_azzeramento,
               d.reso_dt_transazione,
               d.reso_ora_transazione,
               d.cod_transazione
          from DWH_DATI_SCN_STOR d
         where 1 = 1
           and d.tipo_record = 3
           and d.scontrino_prog in
               (select  d.scontrino_prog
                  from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
                 where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
                   and d.tipo_record in (3, 5)
                   and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
                   and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                   and t.caricato = 'Y'
                   and f.filtro_id in ('P')
                 group by d.scontrino_prog
                having count(*) > 1)
   ) loop
     INSERT  INTO PROC.RESO
     select cur3.scontrino_prog,
            cur3.cod_pdv,
            cur3.dt_competenza,
            cur3.ora_inizio,
            cur3.num_articoli,
            cur3.tot_netto,
            cur3.cod_cassa,
            cur3.reso_num_cassa,
            cur3.reso_num_transazione,
            cur3.reso_num_azzeramento,
            cur3.reso_dt_transazione,
            cur3.reso_ora_transazione,
            cur3.cod_transazione
     from dual;
     v_count3 := v_count3 +1;
   end loop;
    
   COMMIT;

      dbms_output.put_line('#testate DIAG inserite: '||to_char(v_count));
      dbms_output.put_line('#testate PROC inserite: '||to_char(v_count2));
      dbms_output.put_line('#testate PROC.RESO inserite: '||to_char(v_count3));

      DMS_COM.write_jlt(v_unit, 1, '#testate DIAG inserite: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#testate PROC inserite: '||to_char(v_count2));
      DMS_COM.write_jlt(v_unit, 1, '#testate PROC.RESO inserite: '||to_char(v_count3));


    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
       DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
       DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||SQLERRM);
      dbms_output.put_line('Generata eccezione ' || SQLCODE || ' ' ||SQLERRM);
      
       DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END ScaricaTestata;

  /******************************************************************************
     NAME:       ScaricaFormePagamento
     PURPOSE:    Funzione che scarica le forme di pagamento in tabella (PROC e DIAG)

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        05/12/2011  Massimiliano Modena - Softquattro
     2.0        31/07/2018  M.Modena            gestione nuovo tipo record resi
  *******************************************************************************/
  FUNCTION ScaricaFormePagamento RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'ScaricaFormePagamento';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);
  --ripulisco DIAG.PAGAMENTO
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco DIAG.PAGAMENTO');

  delete DIAG.PAGAMENTO t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');
  --ripulisco PROC.PAGAMENTO
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.PAGAMENTO');

  delete PROC.PAGAMENTO t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');

  --ripulisco PROC.RESO_PAGAMENTO
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.RESO_PAGAMENTO');

  delete PROC.RESO_PAGAMENTO t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');

   --carico DIAG.PAGAMENTO (a partire da testate che hanno almeno una riga associata)
   DMS_COM.write_jlt(v_unit, 1, 'carico DIAG.PAGAMENTO');

   for cur_diag in (
     select /*+ ORDERED */ d.scontrino_prog,
            d.tipo_pagam,
            d.cod_pdv,
            d.dt_competenza,
            NVL(decode(d.cod_cliente,d.cd_fisc_piva,'000000000000',d.cod_cliente),'000000000000') CLI_ID,
            d.flg_cliente,
            d.valore_netto--/100
    from  DWH_DATI_SCN_STOR d
          where d.tipo_record = 1
            and d.scontrino_prog in (select d.scontrino_prog
              from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
             where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
               and d.tipo_record in (0, 2)
               and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
               and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
               and t.caricato = 'Y'
               and f.filtro_id in ('D')
             group by d.scontrino_prog
            having count(*) > 1)) loop
     
    INSERT INTO DIAG.PAGAMENTO
           select cur_diag.scontrino_prog,
                  cur_diag.tipo_pagam,
                  cur_diag.cod_pdv,
                  cur_diag.dt_competenza,
                  case when to_number(cur_diag.cli_id)>0 then to_char(to_number(cur_diag.cli_id)) else cur_diag.cli_id end,
                  cur_diag.flg_cliente,
                  cur_diag.valore_netto,
                  null as ora
             from dual;

             v_count:= v_count +1;
             
   end loop;
     
   
    --carico PROC.PAGAMENTO (a partire da testate che hanno almeno una riga associata)
    DMS_COM.write_jlt(v_unit, 1, 'carico PROC.PAGAMENTO');

    for cur_proc in (
      select /*+ ORDERED */  d.scontrino_prog,
            d.tipo_pagam,
            d.cod_pdv,
            d.dt_competenza,
            NVL(decode(d.cod_cliente,d.cd_fisc_piva,'000000000000',d.cod_cliente),'000000000000') CLI_ID,
            d.flg_cliente,
            d.valore_netto--/100
    from  DWH_DATI_SCN_STOR d
          where d.tipo_record = 1
            and d.scontrino_prog in (select d.scontrino_prog
              from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
             where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
               and d.tipo_record in (0, 2)
               and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
               and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
               and t.caricato = 'Y'
               and f.filtro_id in ('P')
             group by d.scontrino_prog
            having count(*) > 1)) loop
       begin     
            INSERT INTO PROC.PAGAMENTO
            select cur_proc.scontrino_prog,
                   cur_proc.tipo_pagam,
                   cur_proc.cod_pdv,
                   cur_proc.dt_competenza,
                   case when regexp_instr (cur_proc.cli_id, '^[a-zA-Z]')<>0 then '000000000000'
                        when to_number(cur_proc.cli_id)>0 then to_char(to_number(cur_proc.cli_id)) 
                          else cur_proc.cli_id end,
                   cur_proc.flg_cliente,
                   cur_proc.valore_netto,
                   null as ora
              from dual;
      v_count2 := v_count2+1;
      exception when others then 
     DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
       DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||SQLERRM);
       DMS_COM.write_jlt(v_unit, 1, cur_proc.scontrino_prog ||' - '||cur_proc.cod_pdv||' - '||cur_proc.dt_competenza||' - '||cur_proc.tipo_pagam);
      dbms_output.put_line('Generata eccezione ' || SQLCODE || ' ' ||SQLERRM);
      
       DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
   end;
    end loop;


    --carico PROC.RESO_PAGAMENTO (a partire da testate che hanno almeno una riga associata)
    DMS_COM.write_jlt(v_unit, 1, 'carico PROC.RESO_PAGAMENTO');

    for cur_reso in (
      select /*+ ORDERED */  d.scontrino_prog,
            d.tipo_pagam,
            d.cod_pdv,
            d.dt_competenza,
            d.valore_netto--/100
    from  DWH_DATI_SCN_STOR d
          where d.tipo_record = 4
            and d.scontrino_prog in (select d.scontrino_prog
              from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
             where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
               and d.tipo_record in (3, 5)
               and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
               and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
               and t.caricato = 'Y'
               and f.filtro_id in ('P')
             group by d.scontrino_prog
            having count(*) > 1)) loop
            
            INSERT INTO PROC.RESO_PAGAMENTO
            select cur_reso.scontrino_prog,
                   cur_reso.tipo_pagam,
                   cur_reso.cod_pdv,
                   cur_reso.dt_competenza,
                   cur_reso.valore_netto,
                   null as ora
              from dual;
      v_count3 := v_count3+1;
    end loop;


   COMMIT;

      dbms_output.put_line('#FP DIAG inserite: '||to_char(v_count));
      dbms_output.put_line('#FP PROC inserite: '||to_char(v_count2));
      dbms_output.put_line('#FP PROC_RESO inserite: '||to_char(v_count3));

      DMS_COM.write_jlt(v_unit, 1, '#FP DIAG inserite: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#FP PROC inserite: '||to_char(v_count2));
      DMS_COM.write_jlt(v_unit, 1, '#FP PROC_RESO inserite: '||to_char(v_count3));


    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END ScaricaFormePagamento;

  /******************************************************************************
     NAME:       ScaricaRigheEAnomalie
     PURPOSE:    Funzione che scarica le righe buone e anomale in tabella (PROC e DIAG)

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        13/12/2011  Massimiliano Modena - Softquattro
     2.0        31/07/2018  M.Modena            gestione nuovo tipo record resi
  *******************************************************************************/
  FUNCTION ScaricaRigheEAnomalie RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'ScaricaRigheEAnomalie';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count4              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);

  --ripulisco DIAG.RIGA_ANOMALIA
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco DIAG.RIGA_ANOMALIA');

  delete DIAG.RIGA_ANOMALIA t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');
  --ripulisco DIAG.RIGA
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco DIAG.RIGA');

  delete DIAG.RIGA t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');
  --ripulisco PROC.RIGA_ANOMALIA
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.RIGA_ANOMALIA');

  delete PROC.RIGA_ANOMALIA t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');

  --ripulisco PROC.RESO_RIGA_ANOMALIA
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.RESO_RIGA_ANOMALIA');

  delete PROC.RESO_RIGA_ANOMALIA t
  where t.data||lpad(t.neg_id ,4,'0') in (select to_date(substr(p.nome_file, 9), 'rrmmdd')||
                                    substr(p.nome_file, 4, 4)
                             from proc_tmp_file p where caricato='Y');
   
  commit;                          
   --carico DIAG.RIGA_ANOMALIA (a partire da testate che hanno almeno una riga associata)
   DMS_COM.write_jlt(v_unit, 1, 'carico DIAG.RIGA_ANOMALIA');

  INSERT /*+ APPEND */ INTO DIAG.RIGA_ANOMALIA
    select /*+ ORDERED */  d.scontrino_prog,
       d.cod_ean,
       d.tipo_riga,
       d.flag_art_rep,
       d.cod_pdv,
       d.dt_competenza,
       NVL(decode(d.cod_cliente,d.cd_fisc_piva,'000000000000',d.cod_cliente),'000000000000'),
       d.flg_cliente,
       d.dept,
       d.promo_id,
       d.quantita,
       d.peso_per_1000,
       (d.valore_netto_riga),-- / 100,
       d.bollini_riga,
       d.sconto_riga,-- / 100,
       d.sconto_fidelity,-- / 100,
       d.sconto_trx_ventilato,-- / 100,
       'N',                    --FLG_VERIFICATO
       d.cod_ean,
       null as ora
      from DWH_DATI_SCN_STOR d
     where 1 = 1
       and d.tipo_record = 2
       and (d.cod_art_rep is null or d.cod_art_rep='')
       and d.scontrino_prog in
           (select d.scontrino_prog
              from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
             where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
               and d.tipo_record in (0, 2)
               and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
               and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
               and t.caricato = 'Y'
               and f.filtro_id in ('D')
             group by d.scontrino_prog
            having count(*) >= 1);

    v_count:=SQL%rowcount;

    --carico DIAG.RIGA_ANOMALIA (a partire da testate che hanno almeno una riga associata)
    DMS_COM.write_jlt(v_unit, 1, 'carico DIAG.RIGA');

  INSERT /*+ APPEND */ INTO DIAG.RIGA
    select   /*+ ORDERED */ d.scontrino_prog,
           d.cod_art_rep,
           d.tipo_riga,
           d.flag_art_rep,
           d.cod_pdv,
           d.dt_competenza,
           NVL(decode(d.cod_cliente,d.cd_fisc_piva,'000000000000',d.cod_cliente),'000000000000'),
           d.flg_cliente,
           d.dept,
           d.promo_id,
           d.quantita,
           d.peso_per_1000,
           (d.valore_netto_riga),-- / 100,
           d.bollini_riga,
           d.sconto_riga,-- / 100,
           d.sconto_fidelity ,--/ 100,
           d.sconto_trx_ventilato,-- / 100
           null as ora
          from DWH_DATI_SCN_STOR d
         where 1 = 1
           and d.tipo_record = 2
           and d.cod_art_rep is not null
           and d.scontrino_prog in
               (select d.scontrino_prog
                  from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
                 where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
                   and d.tipo_record in (0, 2)
                   and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
                   and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                   and t.caricato = 'Y'
                   and f.filtro_id in ('D')
                 group by d.scontrino_prog
                having count(*) >= 1);

    v_count2:=SQL%rowcount;

    --carico PROC.RIGA_ANOMALIA (a partire da testate che hanno almeno una riga associata)
    DMS_COM.write_jlt(v_unit, 1, 'carico PROC.RIGA_ANOMALIA');

  INSERT /*+ APPEND */ INTO PROC.RIGA_ANOMALIA
      select /*+ ORDERED */ d.scontrino_prog,
       d.cod_ean,
       d.tipo_riga,
       d.flag_art_rep,
       d.cod_pdv,
       d.dt_competenza,
       NVL(decode(d.cod_cliente,d.cd_fisc_piva,'000000000000',d.cod_cliente),'000000000000'),
       d.flg_cliente,
       d.dept,
       d.promo_id,
       sum(d.quantita),
       d.peso_per_1000,
       sum(d.valore_netto_riga),-- / 100,
       d.bollini_riga,
       d.sconto_riga,-- / 100,
       d.sconto_fidelity ,--/ 100,
       d.sconto_trx_ventilato,-- / 100,
       'N',                    --FLG_VERIFICATO
       null,                    --LOG_DATE
       d.cod_ean,
       null as ora
      from DWH_DATI_SCN_STOR d
     where 1 = 1
       and d.tipo_record = 2
       and (d.cod_art_rep is null or d.cod_art_rep='')
       and d.scontrino_prog in
           (select d.scontrino_prog
              from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
             where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
               and d.tipo_record in (0, 2)
               and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
               and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
               and t.caricato = 'Y'
               and f.filtro_id in ('P')
             group by d.scontrino_prog
            having count(*) >= 1)
group by scontrino_prog, cod_ean, tipo_riga, cod_pdv, dt_competenza        
, d.flag_art_rep,      
NVL(decode(d.cod_cliente,d.cd_fisc_piva,'000000000000',d.cod_cliente),'000000000000'),
       d.flg_cliente, 
       d.dept,
       d.promo_id,
       d.peso_per_1000,
       d.bollini_riga,
       d.sconto_riga,-- / 100,
       d.sconto_fidelity ,--/ 100,
       d.sconto_trx_ventilato,-- / 100,
       'N',                    --FLG_VERIFICATO
       null,
       d.cod_ean;     

    v_count3:=SQL%rowcount;

    --carico PROC.RESO_RIGA_ANOMALIA (a partire da testate che hanno almeno una riga associata)
    DMS_COM.write_jlt(v_unit, 1, 'carico PROC.RESO_RIGA_ANOMALIA');

  INSERT /*+ APPEND */ INTO PROC.RESO_RIGA_ANOMALIA
      select /*+ ORDERED */ d.scontrino_prog,
       d.cod_ean,
       d.tipo_riga,
       d.flag_art_rep,
       d.cod_pdv,
       d.dt_competenza,
       d.dept,
       d.promo_id,
       sum(d.quantita),
       d.peso_per_1000,
       sum(d.valore_netto_riga),-- / 100,
       'N',                    --FLG_VERIFICATO
       null,                   --LOG_DATE
       null as ora
      from DWH_DATI_SCN_STOR d
     where 1 = 1
       and d.tipo_record = 5
       and (d.cod_art_rep is null or d.cod_art_rep='')
       and d.scontrino_prog in
           (select d.scontrino_prog
              from DWH_DATI_SCN_STOR d, anag.filtro_negozio f, proc_tmp_file t
             where lpad(f.neg_id ,4,'0') = lpad(d.cod_pdv ,4,'0')
               and d.tipo_record in (3, 5)
               and lpad(d.cod_pdv ,4,'0') = substr(t.nome_file, 4, 4)
               and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
               and t.caricato = 'Y'
               and f.filtro_id in ('P')
             group by d.scontrino_prog
            having count(*) >= 1)
group by scontrino_prog, cod_ean, tipo_riga, cod_pdv, dt_competenza        
, d.flag_art_rep,       
       d.dept,
       d.promo_id,
       d.peso_per_1000,
       'N',                    --FLG_VERIFICATO
       null;     

    v_count4:=SQL%rowcount;

   COMMIT;

      dbms_output.put_line('#R_ANOMALE DIAG inserite: '||to_char(v_count));
      dbms_output.put_line('#RIGHE DIAG inserite: '||to_char(v_count2));
      dbms_output.put_line('#R_ANOMALE PROC inserite: '||to_char(v_count3));
      dbms_output.put_line('#R_ANOMALE RESO inserite: '||to_char(v_count4));

      DMS_COM.write_jlt(v_unit, 1, '#R_ANOMALE DIAG inserite: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#RIGHE DIAG inserite: '||to_char(v_count2));
      DMS_COM.write_jlt(v_unit, 1, '#R_ANOMALE PROC inserite: '||to_char(v_count3));
      DMS_COM.write_jlt(v_unit, 1, '#R_ANOMALE RESO inserite: '||to_char(v_count4));

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END ScaricaRigheEAnomalie;

  /******************************************************************************
     NAME:       ScaricaArtSettCorrente
     PURPOSE:    Funzione che scarica la struttura ArticoloSettimanaCorrente

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        13/12/2011  Massimiliano Modena - Softquattro
     2.0        31/07/2018  M.Modena            gestione nuovo tipo record resi
  *******************************************************************************/
  FUNCTION ScaricaArtSettCorrente RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'ScaricaArtSettCorrente';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);

  --ripulisco PROC.ARTICOLO_SETTIMANA_CORRENTE
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.ARTICOLO_SETTIMANA_CORRENTE');


  --versione ottimizzata
  for cur in (select to_date(substr(p.nome_file, 9), 'rrmmdd') as giorno,
                     substr(p.nome_file, 5, 3)                 as nome
                from proc_tmp_file p
               where caricato = 'Y') 
  loop
     --ripuliamo le ricoperture sulla destinazione
     delete /*+ index(t XART_SETTIMANA_C_DATA,XART_SETTIMANA_NEG_ID) */
     PROC.ARTICOLO_SETTIMANA_CORRENTE t
      where t.data = cur.giorno
        and lpad(t.neg_id,4,'0') = lpad(cur.nome,4,'0');
     v_count:=v_count+SQL%rowcount;
  end loop;

  DMS_COM.write_jlt(v_unit, 1, '#RIGHE SETT CORR cancellate: '||to_char(v_count));
  
  --ripuliamo i record inutili dalla sorgente
  delete proc_articoliunes p
  where   nvl(fs_qta,0) = 0 and
          nvl(fs_peso,0) = 0 and
          nvl(fs_imp,0) = 0 and
          nvl(fs_sconto,0) = 0 and
          nvl(nfs_qta,0) = 0 and
          nvl(nfs_peso,0) = 0 and
          nvl(nfs_imp,0) = 0 and
          nvl(nfs_sconto,0) = 0 and
          nvl(bollini,0) = 0 and
          nvl(fs_sconto_trx,0) = 0 and
          nvl(nsf_sconto_trx,0) = 0 ;

  commit;

   --carico PROC.ARTICOLO_SETTIMANA_CORRENTE
   DMS_COM.write_jlt(v_unit, 1, 'carico PROC.ARTICOLO_SETTIMANA_CORRENTE');
  --inseriamo la sorgente nella destinazione
  INSERT /*+ APPEND */
  INTO PROC.ARTICOLO_SETTIMANA_CORRENTE
   select substr(cod_pdv,2,3),
          dt_competenza,
          art_id,
          promo_id,
          nvl(fs_qta,0),
          nvl(fs_peso,0),
          nvl(fs_imp,0),
          nvl(fs_sconto,0),
          nvl(nfs_qta,0),
          nvl(nfs_peso,0),
          nvl(nfs_imp,0),
          nvl(nfs_sconto,0),
          nvl(bollini,0),
          nvl(fs_sconto_trx,0),
          nvl(nsf_sconto_trx,0),
          cod_ean,
          tipo_doc
     from proc_articoliunes;

    v_count:=SQL%rowcount;



   COMMIT;

      dbms_output.put_line('#RIGHE SETT CORR inserite: '||to_char(v_count));

      DMS_COM.write_jlt(v_unit, 1, '#RIGHE SETT CORR inserite: '||to_char(v_count));

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END ScaricaArtSettCorrente;

  /******************************************************************************
     NAME:       ConsolidaDwStatoScn
     PURPOSE:    Funzione che consolida la DW_STATO con gli SCN processati

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        13/12/2011  Massimiliano Modena - Softquattro
     2.0        07/10/2019  N. Delle Donne
        -- MODIFICHE PER PROGETTO 4s 
           aggiunto settare a F scontini 4s trattati,
           aggiunta mail di wrning PDV 4s che non hanno inviato scontini con data ieri
           (mail di controllo 'pdv doveva invaire scontrini da 4S e non lo ha fatto' in procedura consolida)
     3.0        24/08/2021  M.Modena            gestione nuovo DM righe scontrino con retention
  *******************************************************************************/
  FUNCTION ConsolidaDwStatoScn RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'ConsolidaDwStatoScn';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

-------- MODIFICHE PER PROGETTO 4s
-------variabili per creazione ed invio MAIL
    v_num_scarti    NUMBER := 0;
    PID             NUMBER;
    NUM_GRUPPO_DEST VARCHAR2(10) := '14';   -- 779 x test  14 per produzione
    v_testo         VARCHAR2(4000);
    crlf CONSTANT VARCHAR2(2) := CHR(13) || CHR(10);
    n_num_righe   NUMBER;

--------usato per cursore che esegue update uguali sulle tab json
    v_upd_json       VARCHAR2(4000);

-----gli update sono tutti uguali
-----la tabella PROC_TMP_4S_UT (utility) contiene tabella, dblink, e filtri aggiuntivi
-----il cursore genera un loop che crea automaticamente l'update         
    CURSOR UT_4S is
    select DISTINCT
     trim(TABJSON)    as TABJSON,
     trim(DBLINK)     as DBLINK,
     trim(FILTRI_AGG) as FILTRI_AGG
    from proc_tmp_4s_ut
    ;

  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);

    DMS_COM.write_jlt(v_unit, 1, 'update ANAG.DW_STATO');

   update ANAG.DW_STATO d
      set d.data_load_db_proc = sysdate
    where d.tipo_file = 'SCN'
      and to_char(data_file, 'RRmmdd') || neg_id in
          (select to_char(to_date(substr(p.nome_file, 9), 'rrmmdd'), 'RRmmdd') ||
                  substr(p.nome_file, 5, 3)
             from proc_tmp_file p, anag.filtro_negozio f
            where caricato = 'Y'
              and f.neg_id = substr(p.nome_file, 5, 3)
              and f.filtro_id in ('P'));
              
    v_count:=SQL%rowcount;

    DMS_COM.write_jlt(v_unit, 1, '#SCN PROC processati: '||to_char(v_count));

    update ANAG.DW_STATO d
      set d.data_load_db_diag = sysdate
    where d.tipo_file = 'SCN'
      and to_char(data_file, 'RRmmdd') || neg_id in
          (select to_char(to_date(substr(p.nome_file, 9), 'rrmmdd'), 'RRmmdd') ||
                  substr(p.nome_file, 5, 3)
             from proc_tmp_file p, anag.filtro_negozio f
            where caricato = 'Y'
              and f.neg_id = substr(p.nome_file, 5, 3)
              and f.filtro_id in ('D'));

     v_count:=SQL%rowcount;

   COMMIT;

      dbms_output.put_line('#SCN processati: '||to_char(v_count));

     DMS_COM.write_jlt(v_unit, 1, '#SCN DIAG processati: '||to_char(v_count));


/************************************************************
       -- MODIFICHE PER PROGETTO 4s : INIZIO
           aggiunto settare a F scontini 4s trattati,
           usa cursore UT_4S dichiarato
           --
           creazione di una mail di Warnong con negozi di 4s che non ahnno inviato dati per ieri
************************************************************/ 
--sincronizziamo le sorgenti impostando in stato Working i record nuovi arrivati finora. Se ne arrivano successivamente non vengono considerat
   DMS_COM.write_jlt(v_unit, 1, 'Inizio sincronizzazione ''Fine'' sorgenti: '||to_char(v_sysdate,'dd/mm/yyyy hh24:mi:ss'));
    update gefa.JSON_SCN_TESTATA@dcg_prod            f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate;  n_num_righe := SQL%rowcount;
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_TESTATA');
    update gefa.JSON_SCN_TESTATA_JRET@dcg_prod       f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate;  
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_TESTATA_JRET');
    update gefa.JSON_SCN_RIGHE@dcg_prod              f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_RIGHE');
    update gefa.JSON_SCN_RIGHE_JRET@dcg_prod         f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_RIGHE_JRET');
    update gefa.JSON_SCN_FP@dcg_prod                 f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_FP');
    update gefa.JSON_SCN_FP_JRET@dcg_prod            f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_FP_JRET');
    update gefa.JSON_SCN_IVA@dcg_prod                f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_IVA');
    update gefa.JSON_SCN_TESTATA_CARD@dcg_prod       f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_TESTATA_CARD');
    update gefa.JSON_SCN_TESTATA_CARD_JRET@dcg_prod  f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_TESTATA_CARD_JRET');
    update gefa.JSON_SCN_DETT_CARD@dcg_prod          f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_DETT_CARD');
    update gefa.JSON_SCN_PROMO@dcg_prod              f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_PROMO');
    update gefa.JSON_SCN_PROMO_JRET@dcg_prod         f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_PROMO_JRET');
    update gefa.JSON_SCN_ART_VIRTUALI@dcg_prod       f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_ART_VIRTUALI');
    update gefa.JSON_SCN_ART_VIRTUALI_JRET@dcg_prod  f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_ART_VIRTUALI_JRET');
    update gefa.JSON_SCN_AUTORIZZ_SV@dcg_prod        f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_AUTORIZZ_SV');
    update gefa.JSON_SCN_EAN_NON_CASSA@dcg_prod      f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_EAN_NON_CASSA');
    commit;
   DMS_COM.write_jlt(v_unit, 1, 'Effettuata sincronizzazione ''Fine'' '||n_num_righe||' scontrini sulle sorgenti');
   
    

    
----mail di Warning
    select count(*) into v_num_scarti from
    (select d.neg_id               as NEG_ID, 
            d.data_file            as DT_COMPETENZA
       from anag.dw_stato d,
            filtro_negozio F,      
            (select substr(cd_entity, 3) neg_id,
                    dt_avvio_4s
               from etl.dcg_an_pdv@dcg_prod
              where FLG_4S = 'Y') NEG_4S                                                     -- solo negozi 4s
      where (1 = 1)
        and lpad(NEG_4S.neg_id, 4, '0') = lpad(D.neg_id, 4, '0') and d.tipo_file = 'SCN'     -- solo tipo file=SCN , per negozi da 4s e filtro_negozio.filtro_id = 'P'   
        and lpad(f.neg_id, 4, '0') = lpad(D.neg_id, 4, '0')      and f.filtro_id = 'P'       -- mantenuto legame con filro negozio, solo filtro_id = 'P' 
        and trunc(d.data_file)>=neg_4s. dt_avvio_4s                                          -- solo pdv 4store dopo la data avvio competenza 
        and data_load_db_proc is null                                                        -- data_load_db_proc nulla              
        and trunc(d.data_file) = trunc(sysdate - 1)                                          -- mancano dati di ieri
      order by data_ricezione desc
    );
    ---mail solo se presenti pdv con dt_competenza ieri 
    IF v_num_scarti>0 THEN     
      
      PID := null;
      
      for neg_manc in 
            (select d.neg_id               as NEG_ID, 
                    d.data_file            as DT_COMPETENZA
               from anag.dw_stato d,
                    filtro_negozio F,      
                    (select substr(cd_entity, 3) neg_id,
                            dt_avvio_4s
                       from etl.dcg_an_pdv@dcg_prod
                      where FLG_4S = 'Y') NEG_4S                                                     -- solo negozi 4s
              where (1 = 1)
                and lpad(NEG_4S.neg_id, 4, '0') = lpad(D.neg_id, 4, '0') and d.tipo_file = 'SCN'     -- solo tipo file=SCN , per negozi da 4s e filtro_negozio.filtro_id = 'P'   
                and lpad(f.neg_id, 4, '0') = lpad(D.neg_id, 4, '0')      and f.filtro_id = 'P'       -- mantenuto legame con filro negozio, solo filtro_id = 'P' 
                and trunc(d.data_file)>=neg_4s. dt_avvio_4s                                          -- solo pdv 4store dopo la data avvio competenza 
                and data_load_db_proc is null                                                        -- data_load_db_proc nulla              
                and trunc(d.data_file) = trunc(sysdate - 1)                                          -- mancano dati di ieri
              order by data_ricezione desc
            )
      loop      
        ---scrivo testo per i pdv cmancanti
        v_testo:='Warning: pdv '|| neg_manc.NEG_ID||' non ha inviato dati da 4s per data competenza: '||neg_manc.DT_COMPETENZA ;
        ------->>>>modifica per SVILUPPO
        PID := TESTO_MAIL@dcg_prod(v_testo, PID);
      end loop;
      ------->>>>modifica per SVILUPPO
    SEND_MAIL@dcg_prod('Warning su caricamento scontrino SCN da 4S',PID,NUM_GRUPPO_DEST);

    END IF;
/************************************************************
       -- MODIFICHE PER PROGETTO 4s :FINE
************************************************************/    
    
/************************************************************
       -- MODIFICHE PER GESTIONE NUOVO DATAMART RIGHE SCONTRINO CON RETENTION a 180 gg
           ricarichiamo gli ultimi due giorni di dati sulla colonna DT_CARICAMENTO della tabella DM_RIGHE_SCONTRINO
************************************************************/ 

     --ripuliamo i dati caricati negli ultimi due gg
     DMS_COM.write_jlt(v_unit, 1, 'ripuliamo i dati caricati negli ultimi due gg');
     
      delete from dm_righe_scontrino d
       where dt_caricamento >= trunc(sysdate-1);
     
     DMS_COM.write_jlt(v_unit, 1, SQL%rowcount||' records');
     commit;
     
     --aggiorniamo i dati degli ultimi due gg 
     DMS_COM.write_jlt(v_unit, 1, 'aggiorniamo i dati degli ultimi due gg');
     --aggiungiamo i dati con il cod_cliente spalmato su tutti i tipi record
      insert into dm_righe_scontrino
      select /*+ NO_INDEX(a) */ a.tipo_record              ,
             a.cod_pdv                  ,
             a.cod_transazione          ,
             a.cod_cassa                ,
             a.dt_competenza            ,
             a.ora_inizio               ,
             a.ora_fine                 ,
             n.cod_cliente              ,
             a.flg_cliente              ,
             a.cd_remoto                ,
             a.cd_fisc_piva             ,
             a.classe                   ,
             a.num_articoli             ,
             a.tot_netto                ,
             a.sconto_transaz           ,
             a.sconto_articoli          ,
             a.sconto_transaz_fidelity  ,
             a.sconto_articoli_fidelity ,
             a.bollini_scontrino        ,
             a.tipo_pagam               ,
             a.valore_netto             ,
             a.cod_ean                  ,
             a.cod_art_rep              ,
             a.tipo_riga                ,
             a.tipo_sconto              ,
             a.flag_art_rep             ,
             a.dept                     ,
             a.quantita                 ,
             a.peso_per_1000            ,
             a.valore_netto_riga        ,
             a.sconto_riga              ,
             a.sconto_fidelity          ,
             a.sconto_trx_ventilato     ,
             a.bollini_riga             ,
             a.scn_key                  ,
             a.dt_caricamento           ,
             a.promo_id                 ,
             a.num_sco_cli              ,
             a.tipo_promo_id            ,
             a.scontrino_prog           ,
             a.reso_num_cassa           ,
             a.reso_num_transazione     ,
             a.reso_num_azzeramento     ,
             a.reso_dt_transazione      ,
             a.reso_ora_transazione     ,
             a.flg_introduzione_ora
       from dwh_dati_scn_stor a,
                    (select /*+ NO_INDEX(b) */  cod_pdv,cod_transazione,cod_cassa,dt_competenza,cod_cliente 
                       from dwh_dati_scn_stor b
                      where dt_caricamento >= trunc(sysdate-1)
                        and b.tipo_record in ('0','3')) n 
      where a.dt_caricamento >= trunc(sysdate-1)
      and a.cod_pdv=n.cod_pdv
      and a.cod_transazione=n.cod_transazione
      and a.cod_cassa=n.cod_cassa
      and a.dt_competenza =n.dt_competenza
      and a.dt_competenza<trunc(sysdate+1); --filtriamo via record anomali con data di competenza futura
      /*select * from dwh_dati_scn_stor
       where dt_caricamento >= trunc(sysdate-1);*/
         /*and flg_cliente!=0
         and (cod_cliente!=cd_fisc_piva or (cod_cliente is not null and cd_fisc_piva is null))
         and (cod_cliente like '04879%' or cod_cliente like '00004879%' or cod_cliente like '4879%');*/
     
     DMS_COM.write_jlt(v_unit, 1, SQL%rowcount||' records');
     commit;
     /*DMS_COM.write_jlt(v_unit, 1, 'aggiorniamo i dati caricati con flg_cliente=0');
     --aggiungiamo i casi anomali con flg_cliente=0 o con partita iva=cod_cliente per le carte unes
     insert into dm_righe_scontrino
      select \*+ NO_INDEX(a) *\ a.tipo_record              ,
             a.cod_pdv                  ,
             a.cod_transazione          ,
             a.cod_cassa                ,
             a.dt_competenza            ,
             a.ora_inizio               ,
             a.ora_fine                 ,
             n.cod_cliente              ,
             a.flg_cliente              ,
             a.cd_remoto                ,
             a.cd_fisc_piva             ,
             a.classe                   ,
             a.num_articoli             ,
             a.tot_netto                ,
             a.sconto_transaz           ,
             a.sconto_articoli          ,
             a.sconto_transaz_fidelity  ,
             a.sconto_articoli_fidelity ,
             a.bollini_scontrino        ,
             a.tipo_pagam               ,
             a.valore_netto             ,
             a.cod_ean                  ,
             a.cod_art_rep              ,
             a.tipo_riga                ,
             a.tipo_sconto              ,
             a.flag_art_rep             ,
             a.dept                     ,
             a.quantita                 ,
             a.peso_per_1000            ,
             a.valore_netto_riga        ,
             a.sconto_riga              ,
             a.sconto_fidelity          ,
             a.sconto_trx_ventilato     ,
             a.bollini_riga             ,
             a.scn_key                  ,
             a.dt_caricamento           ,
             a.promo_id                 ,
             a.num_sco_cli              ,
             a.tipo_promo_id            ,
             a.scontrino_prog           ,
             a.reso_num_cassa           ,
             a.reso_num_transazione     ,
             a.reso_num_azzeramento     ,
             a.reso_dt_transazione      ,
             a.reso_ora_transazione     ,
             a.flg_introduzione_ora
       from dwh_dati_scn_stor a,
                    (select \*+ NO_INDEX(b) *\  cod_pdv,cod_transazione,cod_cassa,dt_competenza,cod_cliente 
                       from dwh_dati_scn_stor b
                      where dt_caricamento >= trunc(sysdate-1)
                        and cod_cliente=cd_fisc_piva
                        and cod_cliente!='0000000000000000'
                        and (cod_cliente like '04879%' or cod_cliente like '00004879%' or cod_cliente like '4879%')) n --solo le carte unes
      where a.dt_caricamento >= trunc(sysdate-1)
      and a.cod_pdv=n.cod_pdv
      and a.cod_transazione=n.cod_transazione
      and a.cod_cassa=n.cod_cassa
      and a.dt_competenza =n.dt_competenza;

     DMS_COM.write_jlt(v_unit, 1, SQL%rowcount||' records');
     commit;
     
     update dm_righe_scontrino set cod_cliente=lpad(to_number(cod_cliente),13,'0');
     commit;*/
     
     --aggiorniamo la retention a 180 gg
     DMS_COM.write_jlt(v_unit, 1, 'aggiorniamo la retention a 365 gg');
     
      delete from dm_righe_scontrino d
       where d.dt_competenza < trunc(sysdate-365);
     
     DMS_COM.write_jlt(v_unit, 1, SQL%rowcount||' records');
     commit;
     
    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END ConsolidaDwStatoScn;

   /******************************************************************************
     NAME:       ConsolidaDwStatoRep
     PURPOSE:    Funzione che consolida la DW_STATO con i REP processati

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        16/01/2012  Massimiliano Modena - Softquattro
  *******************************************************************************/
  FUNCTION ConsolidaDwStatoRep RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'ConsolidaDwStatoRep';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;
    v_settimana           DATE;

  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);

    DMS_COM.write_jlt(v_unit, 1, 'Begin');

   insert into ANAG.DW_STATO d
   select distinct cod_pdv,
                dt_inizio_settimana,
                'REP',
                sysdate,
                sysdate,
                null,
                null
   from DWH_DATI_REP_STOR t;

   v_count:=SQL%rowcount;
   dbms_output.put_line('#REP processati: '||to_char(v_count));
   DMS_COM.write_jlt(v_unit, 1, '#REP processati: '||to_char(v_count));

   COMMIT;

   select distinct dt_inizio_settimana
   into v_settimana
   from DWH_DATI_REP_STOR;

   delete DWH_DATI_SCN_STOR s
   where s.dt_competenza < sysdate -40;


     v_count:=SQL%rowcount;
     dbms_output.put_line('#REP processati: '||to_char(v_count));
     DMS_COM.write_jlt(v_unit, 1, 'DWH_DATI_SCN_STOR ripulita');
     DMS_COM.write_jlt(v_unit, 1, '#rec processati: '||to_char(v_count));

   COMMIT;

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END ConsolidaDwStatoRep;

  /******************************************************************************
     NAME:       LoadRepClass
     PURPOSE:    Funzione che carica la struttura RepClass

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        15/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  FUNCTION LoadRepClass(v_data DATE) RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'LoadRepClass';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);

    execute immediate 'truncate table PROC_REP_CLASS';
    execute immediate 'truncate table PROC_CLASS';
    execute immediate 'truncate table PROC_REP_CLIENT';

    DMS_COM.write_jlt(v_unit, 1, 'truncate table PROC_REP_CLASS');
    DMS_COM.write_jlt(v_unit, 1, 'truncate table PROC_CLASS');
    DMS_COM.write_jlt(v_unit, 1, 'truncate table PROC_REP_CLIENT');

     SELECT * BULK COLLECT
       INTO v_RepClass
       from (SELECT C.CLASSE_ID as classe,
                    lpad(R.REP_POS_ID,4,'0') as reparto ,
                    0 as cli_num,
                    0 as importo,
                    D.cod_pdv as cod_pdv,
                    D.dt_data as dt_inizio_settimana
               FROM proc.CLASSE C, anag.REPARTO_POS R,(select distinct cod_pdv,v_data as dt_data from DWH_DATI_SCN_STOR s
                                                         where 1 = 1
                                                         and s.tipo_record = 2
                                                         and s.dt_competenza between v_data and v_data + 6) D
              ORDER BY D.cod_pdv, D.dt_data,R.REP_POS_ID, C.CLASSE_ID);

    SELECT * BULK COLLECT
      INTO v_Class
      from (SELECT CLASSE_ID as classe,
                   0 as cli_num,
                   0 as sco_num,
                   0 as importo,
                   D.cod_pdv as cod_pdv,
                   D.dt_data as dt_inizio_settimana
            FROM proc.CLASSE,(select distinct cod_pdv,v_data as dt_data from DWH_DATI_SCN_STOR s
                                                         where 1 = 1
                                                         and s.tipo_record = 2
                                                         and s.dt_competenza between v_data and v_data + 6) D
            ORDER BY D.cod_pdv, D.dt_data,CLASSE_ID);

    FORALL idx IN 1 .. v_RepClass.COUNT
      INSERT INTO PROC_REP_CLASS
      VALUES
      v_RepClass(idx);

    v_count:=SQL%rowcount;

    FORALL idx IN 1 .. v_Class.COUNT
      INSERT INTO PROC_CLASS
      VALUES
      v_Class(idx);

    v_count2:=SQL%rowcount;

   COMMIT;
   --pulizia della collection non piu' utilizzata
   DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_RepClass');

   v_RepClass.DELETE;

   DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Class');

   v_Class.DELETE;

      dbms_output.put_line('#record caricati nella RepClass: '||to_char(v_count));
      dbms_output.put_line('#record caricati nella Class: '||to_char(v_count2));

      DMS_COM.write_jlt(v_unit, 1, '#record caricati nella RepClass: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#record caricati nella Class: '||to_char(v_count2));

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END LoadRepClass;

  /******************************************************************************
     NAME:       RicavaDatiRep
     PURPOSE:    Funzione che ricava i dati REP a partire dagli SCN

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        15/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  FUNCTION RicavaDatiRep(v_data DATE) RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'RicavaDatiRep';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;
    DEFAULTCLASS   CONSTANT VARCHAR2(3):='9';
    v_classe              VARCHAR2(3):='9'; --classe di default
  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);

    DMS_COM.write_jlt(v_unit, 1, 'truncate table DWH_DATI_REP_STOR');

    execute immediate 'truncate table DWH_DATI_REP_STOR';

     SELECT * BULK COLLECT
       INTO v_Rep
       from (select  cod_pdv,
                     x_data,
                     cod_cliente,
                     reparto_pos,
                     sum(imp_netto),
                     sum(sconto_trx),
                     sum(sconto_art),
                     sum(sconto_trx_fid),
                     sum(sconto_art_fid),
                     count(*),
                     0,
                     null
                from (select d.cod_pdv as cod_pdv,
                             v_data as x_data,
                             case
                               when flg_cliente = 1 then d.cod_cliente
                               else null
                             end as cod_cliente,
                             d.dept as reparto_pos,
                             sum(nvl(d.valore_netto_riga, 0)) as imp_netto,
                             case
                               when flg_cliente = 0 then sum(nvl(d.sconto_trx_ventilato,0))
                               else 0
                             end as sconto_trx,
                             case
                               when flg_cliente = 0 then sum(nvl(d.sconto_riga,0) + nvl(d.sconto_fidelity,0))
                               else 0
                             end as sconto_art,
                             case
                               when flg_cliente = 1 then sum(nvl(d.sconto_trx_ventilato,0))
                               else 0
                             end as sconto_trx_fid,
                             case
                               when flg_cliente = 1 then sum(nvl(d.sconto_riga,0) + nvl(d.sconto_fidelity,0))
                               else 0
                             end as sconto_art_fid,
                             scn_key
                        from DWH_DATI_SCN_STOR d
                       where 1 = 1
                         and d.tipo_record = 2
                         and d.dt_competenza between v_data and
                             v_data + 6
                       group by d.cod_pdv, d.cod_cliente, d.dept, flg_cliente, scn_key)
               group by cod_pdv, x_data,cod_cliente, reparto_pos);


    FORALL idx IN 1 .. v_Rep.COUNT
      INSERT INTO DWH_DATI_REP_STOR
      VALUES
      v_Rep(idx);


    v_count:=SQL%rowcount;


    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Rep');

    v_Rep.DELETE;

    SELECT * BULK COLLECT
      INTO v_Rep
      from (select cod_pdv,
                   v_data as dt_inizio_settimana,
                   cod_cliente,
                   null as rep_pos_id,
                   null as imp_netto,
                   null as sconto_transaz,
                   null as sconto_articoli,
                   null as sconto_transaz_fidelity,
                   null as sconto_articoli_fidelity,
                   null as num_sco_clienti_rep,
                   count(*) as num_sco_clienti,
                   null
              from (select  d.cod_pdv as cod_pdv,
                           v_data as v_data,
                           case
                             when flg_cliente = 1 then
                              d.cod_cliente
                             else
                              null
                           end as cod_cliente,
                           null as reparto_pos,
                           null as sconto_trx,
                           null as sconto_art,
                           null as sconto_trx_fid,
                           null as sconto_art_fid,
                           scn_key
                      from DWH_DATI_SCN_STOR d
                     where 1 = 1
                       and d.tipo_record = 0
                       --and cod_cliente='487910798140'
                       and d.dt_competenza between v_data and v_data + 6
                     group by d.cod_pdv,
                              d.cod_cliente,
                              d.dept,
                              flg_cliente,
                              scn_key)
             group by cod_pdv, v_data, cod_cliente);

    for idx_rep in 1 .. v_Rep.COUNT
        loop

           update DWH_DATI_REP_STOR p
           set p.num_sco_clienti = v_Rep(idx_rep).num_sco_clienti
           where p.cod_pdv   = v_Rep(idx_rep).cod_pdv
             and p.dt_inizio_settimana = v_Rep(idx_rep).dt_inizio_settimana
             and p.cod_cliente         = v_Rep(idx_rep).cod_cliente;

           if(v_Rep(idx_rep).cod_cliente is null) then

           update DWH_DATI_REP_STOR p
           set p.num_sco_clienti = v_Rep(idx_rep).num_sco_clienti,
               p.cod_cliente='000000000000'
           where p.cod_pdv   = v_Rep(idx_rep).cod_pdv
             and p.dt_inizio_settimana = v_Rep(idx_rep).dt_inizio_settimana
             and p.cod_cliente         is null;

           end if;

        end loop;
    commit;

    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Rep');

    v_Rep.DELETE;

    SELECT * BULK COLLECT
      INTO v_Rep
      from DWH_DATI_REP_STOR;

    for idx_rep in 1 .. v_Rep.COUNT
        loop
           begin

               SELECT  CLASSE_ID
                into   v_classe
                FROM    proc.CLIENTE_MESE CM
                WHERE   CM.CLI_ID = v_Rep(idx_rep).cod_cliente
                AND     CM.NEG_ID = v_Rep(idx_rep).cod_pdv
                AND     CM.DATA_INIZIO_MESE = add_months(
                        trunc(v_Rep(idx_rep).dt_inizio_settimana, 'month'),-1) ;

             exception when NO_DATA_FOUND then
               begin

                 SELECT  CLASSE_ID
                  into   v_classe
                  FROM    proc.CLIENTE_MESE CM
                  WHERE   CM.CLI_ID = v_Rep(idx_rep).cod_cliente
                  AND     CM.NEG_ID = v_Rep(idx_rep).cod_pdv
                  AND     CM.DATA_INIZIO_MESE = add_months(
                          trunc(v_Rep(idx_rep).dt_inizio_settimana, 'month'),-2) ;

               exception when NO_DATA_FOUND then
                  --dbms_output.put_line('ERRORE classe '|| v_classe ||' reparto '||v_Rep(idx_rep).rep_pos_id ||' pdv '||v_Rep(idx_rep).cod_pdv ||' non Presenti');
                  v_classe:=DEFAULTCLASS;
               end;
             end;

          update DWH_DATI_REP_STOR p
           set p.classe = v_classe
           where p.cod_pdv   = v_Rep(idx_rep).cod_pdv
             and p.dt_inizio_settimana = v_Rep(idx_rep).dt_inizio_settimana
             and p.cod_cliente         = v_Rep(idx_rep).cod_cliente;

        end loop;

    update DWH_DATI_REP_STOR t
    set cod_cliente=to_number(cod_cliente)
    where cod_cliente like '0%'
    and cod_cliente !='000000000000';

   COMMIT;

      dbms_output.put_line('#record Rep caricati: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#record Rep caricati: '||to_char(v_count));

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END RicavaDatiRep;

  /******************************************************************************
     NAME:       GestisciRep
     PURPOSE:    Funzione che gestisce le operazioni relative ai dati REP

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        16/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  FUNCTION GestisciRep(v_data DATE) RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'GestisciRep';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

    v_cliente             VARCHAR2(16):='ref';
    v_cli_num             NUMBER:=0;
    v_imp_netto           NUMBER:=0;
    v_cod_pdv             varchar2(16);
    v_date                DATE;
    v_classe              varchar2(3):='ref';
    v_reparto             varchar2(4):='ref';
    v_sco_num             NUMBER:=0;

  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);

    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Rep');

    v_Rep.DELETE;
    --aggiornamento struttura RepClass
    SELECT * BULK COLLECT
      INTO v_Rep
      from DWH_DATI_REP_STOR d
      order by cod_pdv, dt_inizio_settimana , classe, rep_pos_id, cod_cliente;


    for idx_rep in 1 .. v_Rep.COUNT
        loop


           if(v_Rep(idx_rep).classe != v_classe or v_Rep(idx_rep).rep_pos_id != v_reparto ) then

             if(v_cliente != 'ref') then --solo dal primo cliente valido
               update proc_rep_class r
               set r.cli_num=v_cli_num,
                   r.importo=v_imp_netto
               where 1=1
                 and r.cod_pdv=v_cod_pdv
                 and r.dt_inizio_settimana=v_date
                 and r.classe=v_classe
                 and r.reparto=v_reparto; --lpaddato con gli 0.

               v_count:=v_count+SQL%rowcount;
             end if;


             v_cliente := nvl(v_Rep(idx_rep).cod_cliente,'000000000000');
             v_cod_pdv := v_Rep(idx_rep).cod_pdv;
             v_date    := v_Rep(idx_rep).dt_inizio_settimana;
             v_classe  := v_Rep(idx_rep).classe;
             v_reparto := v_Rep(idx_rep).rep_pos_id;
             v_cli_num := 1;
             v_imp_netto := v_Rep(idx_rep).imp_netto;

           else
             v_cli_num := v_cli_num + 1;
             v_imp_netto := v_imp_netto + v_Rep(idx_rep).imp_netto;
           end if;
        end loop;

        --ultimo update restante quando il loop e' terminato
        update proc_rep_class r
             set r.cli_num=v_cli_num,
                 r.importo=v_imp_netto
             where 1=1
               and r.cod_pdv=v_cod_pdv
               and r.dt_inizio_settimana=v_date
               and r.classe=v_classe
               and r.reparto=v_reparto; --lpaddato con gli 0.

             v_count:=v_count+SQL%rowcount;
   COMMIT;

      dbms_output.put_line('#record verificati: '||to_char(v_Rep.COUNT));
      dbms_output.put_line('#record RepClass modificati: '||to_char(v_count));

      DMS_COM.write_jlt(v_unit, 1, '#record verificati: '||to_char(v_Rep.COUNT));
      DMS_COM.write_jlt(v_unit, 1, '#record RepClass modificati: '||to_char(v_count));

      --aggiornamento struttura Class
      SELECT * BULK COLLECT
        INTO v_Rep
        from (SELECT cod_pdv,
                     dt_inizio_settimana ,
                     cod_cliente,
                     null as rep_pos_id,
                     sum(imp_netto) as imp_netto,
                     null as sconto_transaz,
                     null as sconto_articoli,
                     null as sconto_transaz_fidelity,
                     null as sconto_articoli_fidelity,
                     null as num_sco_clienti_rep,
                     num_sco_clienti,
                     classe
              from DWH_DATI_REP_STOR d
              where 1=1
              group by cod_pdv, dt_inizio_settimana , classe,num_sco_clienti, cod_cliente);

    v_classe:='ref';
    v_cod_pdv:='ref';
    v_cliente:='ref';
    v_cli_num:=0;
    v_imp_netto:=0;
    v_sco_num:=0;

    for idx_rep in 1 .. v_Rep.COUNT
        loop


           if(v_Rep(idx_rep).classe != v_classe or v_Rep(idx_rep).cod_pdv != v_cod_pdv) then

             if(v_cliente != 'ref') then --solo dal primo cliente valido
               update proc_class r
               set r.cli_num=v_cli_num,
                   r.sco_num=v_sco_num,
                   r.importo=v_imp_netto
               where 1=1
                 and r.cod_pdv=v_cod_pdv
                 and r.dt_inizio_settimana=v_date
                 and r.classe=v_classe; --lpaddato con gli 0.

               v_count:=v_count+SQL%rowcount;
             end if;


             v_cliente   := nvl(v_Rep(idx_rep).cod_cliente,'000000000000');
             v_cod_pdv   := v_Rep(idx_rep).cod_pdv;
             v_date      := v_Rep(idx_rep).dt_inizio_settimana;
             v_classe    := v_Rep(idx_rep).classe;
             v_cli_num   := 1;
             v_sco_num   := v_Rep(idx_rep).num_sco_clienti;
             v_imp_netto := v_Rep(idx_rep).imp_netto;

           else
             v_cli_num := v_cli_num + 1;
             v_sco_num := v_sco_num + v_Rep(idx_rep).num_sco_clienti;
             v_imp_netto := v_imp_netto + v_Rep(idx_rep).imp_netto;
           end if;
        end loop;

        --ultimo update restante quando il loop e' terminato
        update proc_class r
               set r.cli_num=v_cli_num,
                   r.sco_num=v_sco_num,
                   r.importo=v_imp_netto
               where 1=1
                 and r.cod_pdv=v_cod_pdv
                 and r.dt_inizio_settimana=v_date
                 and r.classe=v_classe; --lpaddato con gli 0.

             v_count:=v_count+SQL%rowcount;
   COMMIT;

   dbms_output.put_line('#record verificati: '||to_char(v_Rep.COUNT));
   dbms_output.put_line('#record Class modificati: '||to_char(v_count));

   DMS_COM.write_jlt(v_unit, 1, '#record verificati: '||to_char(v_Rep.COUNT));
   DMS_COM.write_jlt(v_unit, 1, '#record Class modificati: '||to_char(v_count));

   --gestione della struttura RepClient

     insert into PROC_REP_CLIENT
       select t.cod_pdv,
                           t.dt_inizio_settimana,
                           t.cod_cliente,
                           t.rep_pos_id,
                           sum(t.imp_netto),
                           sum(t.num_sco_clienti_rep),
                           sum(t.num_sco_clienti),
                           sum(t.sconto_transaz),
                           sum(t.sconto_articoli),
                           sum(t.sconto_transaz_fidelity),
                           sum(t.sconto_articoli_fidelity),
                           null, --partitaiva
                           null  --classe
                      from DWH_DATI_REP_STOR t
                     group by cod_pdv,
                              dt_inizio_settimana,
                              cod_cliente,
                              rep_pos_id;

    v_count:=SQL%rowcount;

    dbms_output.put_line('#record RepClient inseriti: '||to_char(v_count));

    DMS_COM.write_jlt(v_unit, 1, '#record RepClient inseriti: '||to_char(v_count));

    COMMIT;

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END GestisciRep;

  /******************************************************************************
     NAME:       ScaricaRepClass
     PURPOSE:    Funzione che scarica le le strutture RepClass e Classi

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        21/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  FUNCTION ScaricaRep RETURN VARCHAR2 IS

    v_unit                  VARCHAR2(30) := 'ScaricaRep';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';

    v_count               NUMBER:=0;
    v_count2              NUMBER:=0;
    v_count3              NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_bollini             NUMBER;
    v_bollini_premio      NUMBER;
    v_promo_id            PROC_PROMO.PROMO_ID%TYPE;

  BEGIN
    dbms_output.put_line('******');
    dbms_output.put_line('Begin '||v_unit);
  --ripulisco PROC.CLASSE_REPARTO
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.CLASSE_REPARTO');

  delete PROC.CLASSE_REPARTO t
   where t.data_inizio_settimana in
         (select distinct p.dt_inizio_settimana
            from PROC_REP_CLASS p);
  --ripulisco PROC.CLASSE_SETTIMANA
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.CLASSE_SETTIMANA');

  delete PROC.CLASSE_SETTIMANA t
   where t.data_inizio_settimana in
         (select distinct c.dt_inizio_settimana
            from PROC_CLASS c);
  --ripulisco PROC.CLIENTE_REPARTO
  DMS_COM.write_jlt(v_unit, 1, 'ripulisco PROC.CLIENTE_REPARTO');

  delete PROC.CLIENTE_REPARTO t
   where t.data_inizio_settimana in
         (select distinct r.dt_inizio_settimana
            from PROC_REP_CLIENT r);

   --carico PROC.CLASSE_REPARTO (per importi diversi da zero)
   DMS_COM.write_jlt(v_unit, 1, 'carico PROC.CLASSE_REPARTO');

  INSERT /*+ APPEND */ INTO PROC.CLASSE_REPARTO
    select  cod_pdv,
            dt_inizio_settimana,
            classe,
            to_number(reparto),
            cli_num,
            importo
    from  PROC_REP_CLASS p
    where 1=1
    and importo != 0
    order by cod_pdv,dt_inizio_settimana,reparto,classe ;

    v_count:=SQL%rowcount;

    --carico PROC.CLASSE_SETTIMANA (per importi diversi da zero)
    DMS_COM.write_jlt(v_unit, 1, 'carico PROC.CLASSE_SETTIMANA');

  INSERT /*+ APPEND */ INTO PROC.CLASSE_SETTIMANA
    select  cod_pdv,
            dt_inizio_settimana,
            classe,
            cli_num,
            sco_num,
            importo
    from  PROC_class p
    where 1=1
    and importo != 0
    order by cod_pdv,dt_inizio_settimana,classe;

    v_count2:=SQL%rowcount;

    --carico PROC.CLIENTE_REPARTO (per importi diversi da zero)
    DMS_COM.write_jlt(v_unit, 1, 'carico PROC.CLIENTE_REPARTO');

  INSERT /*+ APPEND */ INTO PROC.CLIENTE_REPARTO
    select  cod_pdv,
            dt_inizio_settimana,
            client,
            to_number(reparto),
            scontrini_rep_pos_num,
            scontrini_num,
            importo,
            r.sconto_transaz_fidelity,
            r.sconto_articoli_fidelity,
            r.sconto_transaz,
            r.sconto_articoli
    from  PROC_REP_CLIENT r
    where 1=1
    and importo != 0
    order by cod_pdv,dt_inizio_settimana,r.client ;

    v_count3:=SQL%rowcount;

   COMMIT;

      dbms_output.put_line('#RepClass inserite: '||to_char(v_count));
      dbms_output.put_line('#Classi inserite: '||to_char(v_count2));
      dbms_output.put_line('#RepClienti inseriti: '||to_char(v_count2));

      DMS_COM.write_jlt(v_unit, 1, '#RepClass inserite: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#Classi inserite: '||to_char(v_count2));
      DMS_COM.write_jlt(v_unit, 1, '#RepClienti inseriti: '||to_char(v_count2));

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END ScaricaRep;

  /******************************************************************************
     NAME:       all_env
     PURPOSE:    Funzione che legge tutti i parametri dalla tabella
  DMS_R3_COM_CFG_ENVIRONMENT

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        09/01/2008  Massimiliano Modena - Softquattro
  *******************************************************************************/
  FUNCTION all_env RETURN DMS_R3_COM_CFG_ENVIRONMENT%ROWTYPE IS

    env DMS_R3_COM_CFG_ENVIRONMENT%ROWTYPE;

    v_unit VARCHAR2(30) := 'DWH_PROC';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';

  BEGIN

    SELECT *
    INTO   env
    FROM   DMS_R3_COM_CFG_ENVIRONMENT
    WHERE  status = 'A';

    RETURN env;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END ALL_ENV;


  /******************************************************************************
     NAME:       DWH_PROC_LOAD_SCN
     PURPOSE:    Procedura che carica i dati degli SCN
                 a partire dai files indicati nella anag.DW_STATO

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        21/11/2011  Massimiliano Modena - Softquattro
     2.0        07/10/2019  N. Delle Donne
        -- MODIFICHE PER PROGETTO 4s caricamento della PROC_TMP_FILE
      aggiunta in join la dcg_am_pdv filtrata solo per neogzi NON 4s
      SOLO i negozi non 4s saranno trattati
  *******************************************************************************/
  PROCEDURE DWH_PROC_LOAD_SCN
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'DWH_PROC_LOAD_SCN';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_DAT_PATH            VARCHAR2(255);
    v_DAT_LISTA_FILE      VARCHAR2(100);
    v_FILE_NAME_LISTA     VARCHAR2(100);
    v_FILE_NAME_SETTIMANA VARCHAR2(100);
    v_CTL_FILE            VARCHAR2(100);
    v_File_Name           VARCHAR2(10); --si usa in proc per nome file
    v_data                VARCHAR2(8);
    v_Nome_File_bck       VARCHAR2(20);
    v_return              NUMBER;
    v_settimana           INTEGER;
    v_sett_corrente       INTEGER;
    v_anno                INTEGER;
    v_anno_loop           INTEGER;
    v_prosegui            NUMBER := 0;
    v_flusso              VARCHAR2(20);
    v_codifica            VARCHAR2(20);
    v_code_repm           VARCHAR2(20);
    v_code_conto          VARCHAR2(20);
    v_code_entity         VARCHAR2(20);
    v_FileHandle          utl_file.file_type;
    v_NewLine             VARCHAR2(200);
    v_cod_pdv             VARCHAR2(4);
    --variabili per creazione ed invio MAIL
    v_num_scarti    NUMBER := 0;
    PID             NUMBER;
    NUM_GRUPPO_DEST VARCHAR2(10) := '14';   -- 779 x test  14 per produzione 
    v_testo         VARCHAR2(4000);
    crlf CONSTANT VARCHAR2(2) := CHR(13) || CHR(10);
    --------------
    v_sysdate       DATE;
    
    
        
    --------------- specifiche per flusso amazon 
    v_cod_transazione number :=0;      
      
    
    
  BEGIN

    r_env := DWH_PROC.All_Env;
    select sysdate into v_sysdate from dual;

  DMS_COM.write_jlt(v_unit, 1, 'truncate table PROC_TMP_FILE');
  DMS_COM.write_jlt(v_unit, 1, 'truncate table PROC_DATI_GREZZI');

  EXECUTE IMMEDIATE 'truncate table PROC_TMP_FILE';
  EXECUTE IMMEDIATE 'truncate table PROC_DATI_GREZZI';

  /******************************************************************
    MODIFICHE PER PROGETTO 4s
    --procedura DWH_PROC_LOAD_SCN
      caricamento della PROC_TMP_FILE
      aggiunta in join la dcg_am_pdv filtrata solo per neogzi NON 4s
      SOLO i negozi non 4s saranno trattati
  ******************************************************************/
  insert into PROC_TMP_FILE
  SELECT   'SCN0'||F.neg_id||'.'||to_char(D.data_file,'RRmmdd'), 'N'
  FROM filtro_negozio F,
       dw_stato D,
       (select substr(cd_entity,3) neg_id,FLG_4S,DT_AVVIO_4S from etl.dcg_an_pdv@dcg_prod ) NEG_4S
  WHERE   F.filtro_id = 'P'
  AND  D.Tipo_file = decode('P','R','REP','SCN')
  AND  F.neg_id = D.neg_id
  AND  NEG_4S.neg_id = D.neg_id
  AND  (NEG_4S.FLG_4S='N' or(FLG_4S='Y' and D.DATA_FILE<DT_AVVIO_4S)) -- pdv senza flag 4s oppure con flag ma data avvio superiore alla data di competenza sulla anag.dw_stato
  AND  decode('P',  'T',data_load_db_test,
        'D',data_load_db_diag,
        'P',data_load_db_proc,
        'R',data_load_db_proc,
        sysdate) is null;

  DMS_COM.write_jlt(v_unit, 1, 'inserted into PROC_TMP_FILE: '||to_char(SQL%rowcount)||' records');

  commit;

  FOR c_files IN (SELECT nome_file
                      FROM   proc_tmp_file t
                      WHERE  1 = 1
                      AND    (caricato IS NULL OR caricato != 'Y')
                      ORDER  BY t.nome_file)
  LOOP

      v_command := '/oracle/app/oracle/product/11.2.0/dbhome_1/bin/sqlldr '||
                 ' USERID=' || r_env.conn_string ||
                 ' CONTROL=/dbdwh2/DW/applicativo/ctl/CTL_proc_scn.ctl' ||
                 ' DATA=/export/home/unesdw/Scontrini/' || c_files.nome_file ||
                 ' LOG=/dbdwh2/DW/applicativo/log/CTL_proc_scn.log' ||
                 ' BAD=/dbdwh2/DW/applicativo/bad/CTL_proc_scn.bad' ||
                 ' DISCARD=/dbdwh2/DW/applicativo/dsc/CTL_proc_scn.dsc' ||
                 ' ERRORS=1000000';
      v_return := DWMSPKG.os_command(v_command);

    /******* SA 20140909_1020 ********/
      -- considero il pdv corrente appena caricato e che ha il flag_elab null o diverso da 'Y' e da 'E', errato
  begin
    DMS_COM.write_jlt(v_unit, 1, 'processing pdv:'||c_files.nome_file);
  select distinct s.cod_pdv 
  into v_cod_pdv
  from PROC_DATI_GREZZI s
  where s.flag_elab IS NULL OR s.flag_elab not in('Y','E');

  if(substr(c_files.nome_file,4,4) <> v_cod_pdv )then
  
    --creazione di una mail di riepilogo (DA ATTIVARE IN PRODUZIONE)

    PID := null;
    v_testo:='Warning: pdv '|| v_cod_pdv||' nei dati degli scontrini diverso dal pdv riportato come nome del file dati:'||c_files.nome_file ;
    PID := TESTO_MAIL@DCG_PROD(v_testo, PID);
    DMS_COM.write_jlt(v_unit, 1, v_testo );
    
    
    SEND_MAIL@DCG_PROD('Warning su caricamento scontrino SCN',PID,NUM_GRUPPO_DEST);

    --- segno come errato il cod_pdv nella PROC_DATI_GREZZI per non leggerlo alla iterazione successiva
    update PROC_DATI_GREZZI s
       set s.flag_elab = 'E'
     where s.cod_pdv = v_cod_pdv;    
     
  
  else 
     
    DMS_COM.write_jlt(v_unit, 1, 'caricati gli scn sulla proc_dati_grezzi per il file '|| c_files.nome_file );
    
     for c in (SELECT distinct substr(s.cod_pdv,2) as cod_pdv,s.dt_competenza
                 FROM PROC_DATI_GREZZI s
                where s.flag_elab IS NULL OR s.flag_elab not in('Y','E')) loop
                
       delete dwh_dati_scn_stor d
        where d.cod_pdv=c.cod_pdv
          and d.dt_competenza=c.dt_competenza;
         
     end loop;
     
      /*delete dwh_dati_scn_stor d
       where '0' || d.cod_pdv || to_char(d.dt_competenza, 'yyyymmdd') IN
             (SELECT distinct s.cod_pdv ||
                              to_char(s.dt_competenza, 'yyyymmdd')
                FROM PROC_DATI_GREZZI s
                where s.flag_elab IS NULL OR s.flag_elab not in('Y','E') );*/

    DMS_COM.write_jlt(v_unit, 1, 'cancellati '||to_char(SQL%rowcount)||' records');
    commit;
     
    insert  into dwh_dati_scn_stor d
    select
     tipo_record                   ,
     substr(cod_pdv,2)             ,
     cod_transazione               ,
     cod_cassa                     ,
     dt_competenza                 ,
     ora_inizio                    ,
     ora_fine                      ,
     cod_cliente                   ,
     flg_cliente                   ,
     cd_remoto                     ,
     case when cd_fisc_piva like '00004879%' then cd_fisc_piva else null end,
     classe                        ,
     num_articoli                  ,
     tot_netto                     ,
     sconto_transaz                ,
     sconto_articoli               ,
     sconto_transaz_fidelity       ,
     sconto_articoli_fidelity      ,
     bollini_scontrino             ,
     tipo_pagam                    ,
     valore_netto                  ,
     to_number(cod_ean)            ,
     cod_art_rep                   ,
     tipo_riga                     ,
     tipo_sconto                   ,
     flag_art_rep                  ,
     dept                          ,
     quantita                      ,
     peso_per_1000                 ,
     valore_netto_riga             ,
     sconto_riga                   ,
     sconto_fidelity               ,
     sconto_trx_ventilato          ,
     bollini_riga                  ,
     to_char(dt_competenza, 'rrmmdd')||cod_pdv||cod_cassa||cod_transazione ,
     v_sysdate                     ,
     null                          ,
     null                          ,
     null                          ,
     null                          ,
     reso_num_cassa                ,
     reso_num_transazione          ,
     reso_num_azzeramento          ,
     reso_dt_transazione           ,
     reso_ora_transazione          ,
     'N'
      from PROC_DATI_GREZZI s
     where(s.flag_elab IS NULL OR s.flag_elab not in('Y','E'))
     and tipo_record!='6'
     --and cod_pdv!='0258'
     ;

    DMS_COM.write_jlt(v_unit, 1, 'inseriti in dwh_dati_scn_stor:'||to_char(SQL%rowcount)||' records');

    commit;

--facciamo una MERGE per i record di tipo 6 che devono sostituire quelli di tipo 1, con la ricodifica delle FP


    MERGE INTO dwh_dati_scn_stor D
    USING (select 
               case when tipo_record='6' then '1' else tipo_record end as tipo_record, --facciamo confluire le FP '6' sulle FP '1'
               substr(cod_pdv,2)             as cod_pdv,
               cod_transazione               ,
               cod_cassa                     ,
               dt_competenza                 ,
               ora_inizio                    ,
               lpad(nvl(f.cod_fp_colonna,'01'),2,'0') as tipo_pagam,
               sum(valore_netto) as valore_netto,
               to_char(dt_competenza, 'rrmmdd')||cod_pdv||cod_cassa||cod_transazione as scn_key,
               v_sysdate                                                             as dt_caricamento
            from PROC_DATI_GREZZI s, --select * from 
                 DCG_CF_MAPPATURE_FP@DCG_PROD f
           where 1=1
           and (s.flag_elab IS NULL OR s.flag_elab not in('Y','E'))
           and tipo_record='6'
           and lpad(to_char(s.tipo_pagam),2,'0')=lpad(to_char(f.cod_tipo_pagamento(+)),2,'0')
           group by tipo_record,
                    cod_pdv,
                    cod_transazione,
                    cod_cassa,
                    dt_competenza,
                    ora_inizio,
                    f.cod_fp_colonna) S
    ON (D.tipo_record = '1' 
        and D.cod_pdv = S.cod_pdv 
        and D.cod_transazione = S.cod_transazione
        and D.cod_cassa = S.cod_cassa
        and D.dt_competenza = S.dt_competenza
        and D.tipo_pagam = S.tipo_pagam)
    WHEN MATCHED THEN
      UPDATE SET D.valore_netto = S.valore_netto
    WHEN NOT MATCHED THEN
      INSERT
        (D.tipo_record,
         D.cod_pdv,
         D.cod_transazione,
         D.cod_cassa,
         D.dt_competenza,
         D.ora_inizio,
         D.tipo_pagam,
         D.valore_netto,
         D.flg_cliente,
         D.Scn_Key,
         D.Dt_Caricamento)
      VALUES
        (S.tipo_record,
         S.cod_pdv,
         S.cod_transazione,
         S.cod_cassa,
         S.dt_competenza,
         S.ora_inizio,
         S.tipo_pagam,
         S.valore_netto,
         0,
         S.Scn_Key,
         S.Dt_Caricamento);
        
    DMS_COM.write_jlt(v_unit, 1, 'inseriti in dwh_dati_scn_stor:'||to_char(SQL%rowcount)||' records di tipo 6');
    commit;
    
    
     
    UPDATE proc_tmp_file
        SET caricato = 'Y'
      WHERE nome_file = c_files.nome_file;
     
  -- aggiorno il flag per indicare che il pdv corrente e' stato caricato 
      update PROC_DATI_GREZZI s
      set s.flag_elab = 'Y'
      where s.cod_pdv = v_cod_pdv;
  
     COMMIT;

     
    /******* FINE SA 20140909_1020 ********/
      
  end if;  
  
     

     COMMIT;
  exception 
    WHEN no_data_found THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Warning!! pdv non trovato  ' || v_cod_pdv ); 
      
    WHEN OTHERS THEN --per gestire gli altri errori, anche il to_number(cod_ean)
      PID := null;
      
      v_testo:='Warning: pdv '|| v_cod_pdv||' con errore nei dati degli scontrini nel file dati:'||c_files.nome_file ;
      PID := TESTO_MAIL@DCG_PROD(v_testo, PID);
      DMS_COM.write_jlt(v_unit, 1, v_testo );
      
      v_testo:='Generata eccezione ' || SQLCODE || ' ' ||SQLERRM;
      PID := TESTO_MAIL@DCG_PROD(v_testo, PID);
      DMS_COM.write_jlt(v_unit, 1, v_testo );
    
    
      SEND_MAIL@DCG_PROD('Warning su caricamento scontrino SCN',PID,NUM_GRUPPO_DEST);
      
      
      --- segno come errato il cod_pdv nella PROC_DATI_GREZZI per non leggerlo alla iterazione successiva
      
      rollback; --facciamo il rollback delle eventuali transazioni in sospeso
      update PROC_DATI_GREZZI s
         set s.flag_elab = 'E'
       where s.cod_pdv = v_cod_pdv; 
      
      commit;   
  end;

  END LOOP;
  
  
  


  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DWH_PROC_LOAD_SCN;
  
  
 PROCEDURE DWH_PROC_LOAD_SCN_AMZ
  (
    P_Job    In Varchar2
   ,P_Period In Varchar2
   ,P3       In Varchar2 Default Null
   ,P4       In Varchar2 Default Null
   ,P5       In Varchar2 Default Null
   ,P6       In Varchar2 Default Null
   ,P7       In Varchar2 Default Null
   ,P8       In Varchar2 Default Null
   ,P9       In Varchar2 Default Null
   ,P10      In Varchar2 Default Null
   ,P11      In Varchar2 Default Null
  ) Is

    V_Unit Varchar2(30) := 'DWH_PROC_LOAD_SCN_AMZ';
    K_Error_Marker Constant Char(22) := '[*** !!! ***]  [ERROR]';
    N_Num_Righe           Number;
    R_Env                 Dms_Com_Cfg_Environment%Rowtype;
    V_Command             Varchar2(1000);
    V_Dat_Path            Varchar2(255);
    V_Dat_Lista_File      Varchar2(100);
    V_File_Name_Lista     Varchar2(100);
    V_File_Name_Settimana Varchar2(100);
    V_Ctl_File            Varchar2(100);
    V_File_Name           Varchar2(10); --Si Usa In Proc Per Nome File
    V_Data                Varchar2(8);
    V_Nome_File_Bck       Varchar2(20);
    V_Return              Number;
    V_Settimana           Integer;
    V_Sett_Corrente       Integer;
    V_Anno                Integer;
    V_Anno_Loop           Integer;
    V_Prosegui            Number := 0;
    V_Flusso              Varchar2(20);
    V_Codifica            Varchar2(20);
    V_Code_Repm           Varchar2(20);
    V_Code_Conto          Varchar2(20);
    V_Code_Entity         Varchar2(20);
    V_Filehandle          Utl_File.File_Type;
    V_Newline             Varchar2(200);
    V_Cod_Pdv             Varchar2(4);
    --Variabili Per Creazione Ed Invio Mail
    V_Num_Scarti    Number := 0;
    Pid             Number;
    Num_Gruppo_Dest Varchar2(10) := '14';   -- 779 x test 14 per produzione
    V_Testo         Varchar2(4000);
    Crlf Constant Varchar2(2) := Chr(13) || Chr(10);
    --------------
    V_Sysdate       Date;
    
    
    --------------- Specifiche Per Flusso Amazon 
    V_Cod_Transazione Number :=0;  
    v_cod_ean         number(13) :=0;    
    v_nrec            number:=0; 
    v_nodata_stato    Exception; 
    
  Begin   
  
  R_Env := Dwh_Proc.All_Env;
  Select Sysdate Into V_Sysdate From Dual;
  
 -- 3.3.1 Ricostruzione Dei Record In Anag.Dw_Stato
  
  -- 1) Aggiungiamo Un Record Per Ogni Occorrenza Distinta Della Combinazione Cod_Pdv/Data_Vendita 
  -- Presente Nella Tabella Dcg_Dm_Order_Reports Che Contiene Gli Incassi Per Un Singolo Pdv 
  -- In Modo Da Riprodurre La Presenza Di Un File Per Ciascuna Combinazione Di Cui Si
  -- Andranno Poi A Caricare I Dati Di Incasso Come Se Fossero Scontrini:

Dms_Com.Write_Jlt(V_Unit, 1, 'Inserimento dati in dw_stato');

  -- NON DOBBIAMO PROCESSARLO DUE VOLTE, INSERIRE CONTROLLO  ok, vedi riga 3061 
  -- qui infatti alla fine deve inserire la data di caricamento terminato  
  
    
   
  for c in (Select Distinct 
              Lpad(Trim(Cod_Pdv),3,'0') Neg, 
              Trunc(To_Date(Data_Vendita,'DD/MM/YYYY')) Data_Vendita,
              'SCN' Tipo_File ,
              Sysdate Data_Ricezione,Null as data_load_db_proc,Null as data_load_db_diag,Null as data_load_db_test  
              From Dcg_Dm_Order_Reports@Dcg_PROD d, anag.filtro_negozio f
              where 1=1
              and nvl(Utilizzato_Dwh,'N') <>'Y' 
              and Lpad(Trim(Cod_Pdv),3,'0')=f.neg_id and f.filtro_id='P') loop
        
        begin
          Insert Into Dw_Stato
            select c.neg,
                   c.data_vendita,
                   c.tipo_file,
                   c.data_ricezione,
                   c.data_load_db_proc,
                   c.data_load_db_diag,
                   c.data_load_db_test
              from dual;
        
          v_nrec := v_nrec + SQL%rowcount;
        exception
          when others then
            Dms_Com.Write_Jlt(V_Unit, 1, 'Warning!!! eccezione insert dw_stato:'||to_char(c.neg)||','||to_char(c.data_vendita,'dd/mm/yyyy'));
        end;
        
  end loop;          
 
          
        
     Commit;
  
   
  if v_nrec = 0 then  
     Dms_Com.Write_Jlt(V_Unit, 1, 'Warning!!! Nessun record inserito in dw_stato');
 
  else      
     Dms_Com.Write_Jlt(V_Unit, 1, 'Ok: Record inseriti in dw_stato:'||v_nrec);
   
  
  
  /*--  a questo punto pero per tutti i records che invece sono presenti nella dw_stato e che nella order_reoprts 
  -- si tenta di caricare una seconda volta va eseguito un updat e
  for c_anom in (
   Select distinct cod_pdv,data_vendita From  Dw_Stato S,Dcg_Dm_Order_Reports@Dcg_PROD O
    Where S.Neg_Id = Lpad(Trim(O.Cod_Pdv),3,'0') And  
      S.Data_File = Trunc(To_Date(O.Data_Vendita,'DD/MM/YYYY'))
       And  Decode('P',  'T',Data_Load_Db_Test,
                                 'D',Data_Load_Db_Diag,
                                 'P',Data_Load_Db_Proc,
                                 'R',Data_Load_Db_Proc,
                                  Sysdate) Is Not Null
   ) loop                                
  
   --ha perso la sua utilit? nel tempo
   Update Dcg_Dm_Order_Reports@Dcg_PROD 
      Set anomalie = 'Periodo a cui si riferisce ordine gia caricato nel flusso SCN'
      Where Cod_Pdv = c_anom.cod_pdv
      and data_vendita = c_anom.data_vendita;
  
  end loop ;*/
  

  --3.3.2 Applicazione Delle Trasformazioni Degli Scontrini Ai Nuovi Dati Di Incasso


  -- Truncate Prima Cosi Si Evita Di Inserirli Due Volte
  -- Execute Immediate 'truncate table PROC_TMP_FILE';
  -- eseguo una delete perche la amazon gira dopo e non voglio cancellare l'elenco degliscn caricati prima 
  delete from PROC_TMP_FILE where substr(nome_file,1,7) ='SCN0195';
  delete from PROC_TMP_FILE where substr(nome_file,1,7) ='SCN0196';
  delete from PROC_TMP_FILE where substr(nome_file,1,7) ='SCN0203';
  Dms_Com.Write_Jlt(V_Unit, 1, 'delete table PROC_TMP_FILE');

  
  Execute Immediate 'truncate table PROC_DATI_GREZZI';
  Dms_Com.Write_Jlt(V_Unit, 1, 'truncate table PROC_DATI_GREZZI');

  Execute Immediate 'truncate table PROC_TMP_EAN';
  Dms_Com.Write_Jlt(V_Unit, 1, 'truncate table PROC_TMP_EAN');



  Insert Into Proc_Tmp_File
         Select   'SCN0'||F.Neg_Id||'.'||To_Char(D.Data_File,'RRmmdd'), 'N'
                  From   Filtro_Negozio F,
                         Dw_Stato D
                  Where   F.Filtro_Id = 'P'
                     and  f.neg_id in (select substr(cd_entity,3) 
                                         from dcg_an_pdv@Dcg_PROD 
                                        where flg_amazon='Y' /*'195','196','203'*/
                                      )  -- ###########  SOLO AMAZON  
                     And  D.Tipo_File = 'SCN'
                     And  F.Neg_Id = D.Neg_Id
                     And  Decode('P',  'T',Data_Load_Db_Test,
                                 'D',Data_Load_Db_Diag,
                                 'P',Data_Load_Db_Proc,
                                 'R',Data_Load_Db_Proc,
                                  Sysdate) Is Null;

 
         v_nrec:=SQL%rowcount;
        
     Commit;
   
  if v_nrec = 0 then  
     Dms_Com.Write_Jlt(V_Unit, 1, 'Warning!!! Nessun record inserito in PROC_TMP_FILE:');
  else      
     Dms_Com.Write_Jlt(V_Unit, 1, 'Ok: Record inseriti in PROC_TMP_FILE:'||v_nrec);
  end if ;    
  


  -- Tabella necessaria per eliminare i casi in cui ad un articolo corrispondono piu ean
  -- come da telefonata con cesare, verra preso quello con valore di ean_id piu alto 
  
  --  Anag.Ean Ean,   !!! Ripristinare Inproduzione al posto di  S4_Ean@Dwh_To_Dcc Ean,
  
  Insert Into Proc_Tmp_Ean
         Select Max(Ean_Id),Art_Id,'' From  ANAG.EAN Group By Art_Id;
         V_Nrec:=Sql%Rowcount;
     Commit;
   
  If V_Nrec = 0 Then  
     Dms_Com.Write_Jlt(V_Unit, 1, 'Warning!!! Nessun record inserito in PROC_TMP_EAN:');
  Else      
     Dms_Com.Write_Jlt(V_Unit, 1, 'Ok: Record inseriti in PROC_TMP_EAN:'||V_Nrec);
  End If ;    





  V_Cod_Transazione := 1;  -- per ogni pdv/giorno  rappresentato dai records nella tabella proc_tmp_file  NEL CICLO DI FOR 
                           -- IL DUALIASMO e RAPPRESENTATO GIa NEL NOME DEL FILE CHE CONTIENE SIA IL PDV CHE IL GIORNO DA CARICARE 

  -- ciclo  esterno, loop su pdv e data 

  For C_Files In (Select T.Nome_File
                      From   Proc_Tmp_File T 
                      where substr(nome_file,1,7) in (select 'SCN0'||substr(cd_entity,3) 
                                                        from dcg_an_pdv@Dcg_PROD 
                                                       where flg_amazon='Y'/*'SCN0195','SCN0196','SCN0203'*/
                                                     )  -- #### PROCESSO SOLO AMAZON           
                    Order  By T.Nome_File
                  )
  Loop


  -- Tipo Record = 0 
  -- Qui Devo Inserire In Procedure Dati Grezzi Simulando Il Ctl File Di Sqlloader
  -- ciclo 1 per le tipologie records 0 e 1 
 

  -- per ogni pdv e giorno ora ordine esegue un loop sulla tabella order_reports 
  -- per estrarre il numero della transazione 
  For C_Records In (Select Distinct Lpad(Trim(Cod_Pdv),3,'0') Cod_Pdv,
                           To_Char(To_Date(Data_Vendita,'dd/mm/yyyy'),'yymmdd') Data_Vendita,
                           Ora_Vendita,num_ordine 
                    From Dcg_Dm_Order_Reports@Dcg_PROD
     Where 1=1
           And Lpad(Trim(Cod_Pdv),3,'0') = Substr(C_Files.Nome_File,5,3)  
           And To_Char(To_Date(Data_Vendita,'dd/mm/yyyy'),'yymmdd')  = Substr(C_Files.Nome_File,9,6))
  Loop

  -- -------------------------
  -- tipo record 0 
  -- -------------------------
  Insert Into Proc_Dati_Grezzi
  (Tipo_Record,   Cod_Pdv         ,   Cod_Transazione ,   Cod_Cassa  ,   Dt_Competenza  ,   Ora_Inizio      ,
   Cod_Cliente    ,   Flg_Cliente ,   Cd_Remoto ,   Cd_Fisc_Piva ,   Classe,   Num_Articoli  ,   Tot_Netto    ,
   Sconto_Transaz   ,   Sconto_Articoli   ,   Sconto_Transaz_Fidelity,   Sconto_Articoli_Fidelity,   Bollini_Scontrino)
  Select 
   '0' tipo_record,
   Lpad(Trim(Cod_Pdv),4,'0') Cod_Pdv         ,
   V_Cod_Transazione Cod_Transazione,
   '0001' Cod_Cassa  ,
   To_Date(Data_Vendita,'dd/mm/yyyy') Dt_Competenza  ,
   Ora_Vendita Ora_Inizio      ,
   Null Cod_Cliente    ,
   0 Flg_Cliente ,
   0 Cd_Remoto ,
   Null Cd_Fisc_Piva ,
   '000' Classe,
   Sum(Qta_Pezzi) Num_Articoli  ,
   Sum(Venduto_Tot) Tot_Netto    ,
   0 Sconto_Transaz   ,
   Sum(Sconto_Tot) Sconto_Articoli   ,
   0 Sconto_Transaz_Fidelity,
   0 Sconto_Articoli_Fidelity,
   0 Bollini_Scontrino
  From Dcg_Dm_Order_Reports@Dcg_PROD
     Where 1=1
           And Lpad(Trim(Cod_Pdv),3,'0') = C_records.cod_pdv  
           And To_Char(To_Date(Data_Vendita,'dd/mm/yyyy'),'yymmdd')  = C_records.data_vendita
           And ora_vendita = C_records.ora_vendita
           AND num_ordine = C_records.num_ordine
     Group By 
           Cod_Pdv,
           To_Date(Data_Vendita,'dd/mm/yyyy'),
           ora_vendita,num_ordine;

  --Dms_Com.Write_Jlt(V_Unit, 1, 'Amazon ins type 0 into Proc_Dati_Grezzi: Nrec:'||To_Char(Sql%Rowcount)||' N.Trans: '||V_Cod_Transazione );
  Commit;


  -- -------------------------
  -- tipo record 1 
  -- -------------------------
  -- Bisogna Creare Un Record Per Ogni Pdv/Data/Ordine

  Insert Into Proc_Dati_Grezzi
  (Tipo_Record,Cod_Pdv,
   Cod_Transazione ,
   Cod_Cassa  ,
   Dt_Competenza  ,
   Ora_Inizio      ,
   Tipo_Pagam   ,
   Valore_Netto)
  Select 
   '1'  Tipo_Record,
   Lpad(Trim(Cod_Pdv),4,'0') Cod_Pdv         ,
   V_Cod_Transazione Cod_Transazione ,
   '0001' Cod_Cassa  ,   
    To_Date(Data_Vendita,'dd/mm/yyyy') Dt_Competenza  ,
   Ora_Vendita Ora_Inizio      ,
   '01' Tipo_Pagam,  
   Sum(Venduto_Tot) Valore_Netto 
   From Dcg_Dm_Order_Reports@Dcg_PROD 
     Where 1=1
           And Lpad(Trim(Cod_Pdv),3,'0') = C_records.cod_pdv  
           And To_Char(To_Date(Data_Vendita,'dd/mm/yyyy'),'yymmdd')  = C_records.data_vendita
           And ora_vendita = C_records.ora_vendita
           AND num_ordine = C_records.num_ordine
     Group By 
           Cod_Pdv,
            To_Date(Data_Vendita,'dd/mm/yyyy')    ,
            ora_vendita,
            num_ordine ;
 

  --Dms_Com.Write_Jlt(V_Unit, 1, 'Amazon ins type 1 into Proc_Dati_Grezzi: Nrec:'||To_Char(Sql%Rowcount)||' N.Trans: '||V_Cod_Transazione );
  Commit; 

 
  -- -------------------------
  -- tipo record 2
  -- -------------------------

  -- Tiporecord?  = 2 : Bisogna Creare 1 Record Per Pdv/Data/Ordine/Articolo
  -- select art_cod, '00'||lpad(substr(reparto_cod,3,2),2,'0') from com_cdi_art_dit@dwh_to_dw
 

   Insert Into Proc_Dati_Grezzi
  (Tipo_Record,Cod_Pdv,Cod_Transazione,Cod_Cassa,Dt_Competenza,Ora_Fine,
   Cod_Ean,Cod_Art_Rep,Tipo_Riga,Tipo_Sconto,Flag_Art_Rep,Dept,
   Quantita,Peso_Per_1000,Valore_Netto_Riga,   
   Sconto_Riga   ,
   Sconto_Articoli_Fidelity,
   Sconto_Trx_Ventilato,
   Bollini_Riga)
  Select 
   '2'  Tipo_Record,
   Lpad(Trim(Cod_Pdv),4,'0') Cod_Pdv         ,
   V_Cod_Transazione Cod_Transazione ,
   '0001' Cod_Cassa  ,
    To_Date(Data_Vendita,'dd/mm/yyyy') Dt_Competenza,
   Ora_Vendita Ora_fine,
   nvl(ean.ean_id,'-1') Cod_Ean,  -- SE NON TROVATO METTO A NULL PER FARLO INTERCETTARE NELLA PROCESSING  
   Decode(Nvl(Ean.Ean_Id,''),'','',Lpad(Cod_Articolo_Unes,6,'0')) Cod_Art_Rep , --6 Cifre Con Zeri In Testa, Se Non Esiste L'Ean Corrispondente Lo Metto Comunque A Blank
--   Lpad(Cod_Articolo_Unes,6,'0') Cod_Art_Rep , --6 Cipre Con Zeri In Testa, Se Non Esiste L'Ean Corrispondente Lo Metto Comunque A Blank
   '1' Tipo_Riga,
   Decode(Nvl(Sconto_Tot,'0'),'0','000','009') Tipo_Sconto, --Senon Valorizzato 000 Altrimenti 009
   'A' Flag_Art_Rep,
--   '00'||Lpad(Substr(Nvl(Cdi.Reparto_Cod,'0001'),3,2),2,'0') Dept,  RETTIFICATO, VEDIRIGA SOTTO    
   Lpad(Substr(  Nvl(   Decode(Cdi.Reparto_Cod,'nd','001',Cdi.Reparto_Cod),'001'),2,2) ,4,'0') Dept, --- Questo Va Ricavato Dalla Com_Cdi_Art_Dit Codice Del Settore Vedi Specifica    
   Sum(Qta_Pezzi) Quantita,  
--   Sum(Qta_Kg/1000) Peso_Per_1000,  -- il valore arriva in GRAMMI non lo divido piu
   Sum(Qta_Kg) Peso_Per_1000,  -- il valore arriva in GRAMMI non lo divido piu
   Sum(Venduto_Tot) Valore_Netto_Riga,
   Sum(Sconto_Tot) Sconto_Riga,
   0 Sconto_Articoli_Fidelity,
   0 Sconto_Trx_Ventilato,
   0 Bollini_Riga
  From Dcg_Dm_Order_Reports@Dcg_PROD,
      Proc_Tmp_Ean Ean,
      Com_Cdi_Art_Dit@dwh_to_dcc Cdi
     Where 1=1
           And Lpad(Trim(Cod_Pdv),3,'0') = C_records.cod_pdv  
           And To_Char(To_Date(Data_Vendita,'dd/mm/yyyy'),'yymmdd')  = C_records.data_vendita
           And ora_vendita = C_records.ora_vendita
           AND num_ordine = C_records.num_ordine
       and cdi.last_flg (+) = 1 -- prendo sempre l'ultimo valido 
       and Cod_Articolo_Unes = ean.art_id (+) 
       and Cod_Articolo_Unes = cdi.art_cod (+)
       Group By 
           Cod_Pdv,
            To_Date(Data_Vendita,'dd/mm/yyyy')    ,
            ora_vendita,
            num_ordine,
           Lpad(Cod_Articolo_Unes,6,'0') ,
           ean.ean_id,
            Lpad(Substr(  Nvl(   Decode(Cdi.Reparto_Cod,'nd','001',Cdi.Reparto_Cod),'001'),2,2) ,4,'0') ,
            Decode(Nvl(Sconto_Tot,'0'),'0','000','009');
  
  
  --Dms_Com.Write_Jlt(V_Unit, 1, 'Amazon ins type 2 into Proc_Dati_Grezzi: Nrec:'||To_Char(Sql%Rowcount)||' N.Trans: '||V_Cod_Transazione );
  Commit; 

  V_Cod_Transazione := V_Cod_Transazione + 1;

  -- Fine Loop Secondario Per Generare Il Progressivo della transazione   
  end loop ;

 
  -- test per verificare che effettivamente i files siano sstati processati, in caso contrario manda una e-mail 
  begin 
  
   Select Distinct S.Cod_Pdv 
     Into V_Cod_Pdv
       From Proc_Dati_Grezzi S
          Where S.Flag_Elab Is Null Or S.Flag_Elab Not In('Y','E');

  
    If(Substr(C_Files.Nome_File,4,4) <> V_Cod_Pdv )Then
 
    --Creazione Di Una Mail Di Riepilogo (Da Attivare In Produzione)
    Pid := Null;
    V_Testo:='Warning: pdv '|| V_Cod_Pdv||' nei dati degli scontrini diverso dal pdv riportato come nome del file dati:'||C_Files.Nome_File ;
    Pid := Testo_Mail@Dcg_Prod(V_Testo, Pid);
    Dms_Com.Write_Jlt(V_Unit, 1, V_Testo );
    Send_Mail@Dcg_Prod('Warning su caricamento scontrino SCN',Pid,Num_Gruppo_Dest);

    --- Segno Come Errato Il Cod_Pdv Nella Proc_Dati_Grezzi Per Non Leggerlo Alla Iterazione Successiva
    Update Proc_Dati_Grezzi S
       Set S.Flag_Elab = 'E'
     Where S.Cod_Pdv = V_Cod_Pdv;    
  
    Else 
     
    Dms_Com.Write_Jlt(V_Unit, 1, 'caricati gli scn sulla proc_dati_grezzi per il file '|| C_Files.Nome_File );
    
      Delete Dwh_Dati_Scn_Stor D
       Where '0' || D.Cod_Pdv || To_Char(D.Dt_Competenza, 'yyyymmdd') In
             (Select Distinct S.Cod_Pdv ||
                              To_Char(S.Dt_Competenza, 'yyyymmdd')
                From Proc_Dati_Grezzi S
                Where S.Flag_Elab Is Null Or S.Flag_Elab Not In('Y','E') );

    Dms_Com.Write_Jlt(V_Unit, 1, 'cancellati '||To_Char(Sql%Rowcount)||' records');
    Commit;

    Insert  Into Dwh_Dati_Scn_Stor D
    Select
     Tipo_Record                   ,
     Substr(Cod_Pdv,2)             ,
     Cod_Transazione               ,
     Cod_Cassa                     ,
     Dt_Competenza                 ,
     Ora_Inizio                    ,
     Ora_Fine                      ,
     Cod_Cliente                   ,
     Flg_Cliente                   ,
     Cd_Remoto                     ,
     Cd_Fisc_Piva                  ,
     Classe                        ,
     Num_Articoli                  ,
     Tot_Netto                     ,
     Sconto_Transaz                ,
     Sconto_Articoli               ,
     Sconto_Transaz_Fidelity       ,
     Sconto_Articoli_Fidelity      ,
     Bollini_Scontrino             ,
     Tipo_Pagam                    ,
     Valore_Netto                  ,
     To_Number(Cod_Ean)            ,
     Cod_Art_Rep                   ,
     Tipo_Riga                     ,
     Tipo_Sconto                   ,
     Flag_Art_Rep                  ,
     Dept                          ,
     Quantita                      ,
     Peso_Per_1000                 ,
     Valore_Netto_Riga             ,
     Sconto_Riga                   ,
     Sconto_Fidelity               ,
     Sconto_Trx_Ventilato          ,
     Bollini_Riga                  ,
     To_Char(Dt_Competenza, 'rrmmdd')||Cod_Pdv||Cod_Cassa||Cod_Transazione ,
     V_Sysdate                     ,
     Null                          ,
     Null                          ,
     Null                          ,
     Null                          ,
     reso_num_cassa                ,
     reso_num_transazione          ,
     reso_num_azzeramento          ,
     reso_dt_transazione           ,
     reso_ora_transazione          ,
      'N'
    From Proc_Dati_Grezzi S
      Where S.Flag_Elab Is Null Or S.Flag_Elab Not In('Y','E');

    Dms_Com.Write_Jlt(V_Unit, 1, 'inseriti in dwh_dati_scn_stor:'||To_Char(Sql%Rowcount)||' records');

    Update Proc_Tmp_File
        Set Caricato = 'Y'
      Where Nome_File = C_Files.Nome_File;

      -- Aggiorno Il Flag Per Indicare Che Il Pdv Corrente E' Stato Caricato 
    Update Proc_Dati_Grezzi S
      Set S.Flag_Elab = 'Y'
      Where S.Cod_Pdv = V_Cod_Pdv;
      
    --Ii devono impostare i valori UTILIZZATO_DWH=?Y? e
    --DATA_UTILIZZO_DWH=sysdate per i dati utilizzati dalla tabella DCG_DM_ORDER_REPORTS  
    Update Dcg_Dm_Order_Reports@dcg_prod 
      Set Utilizzato_Dwh='Y',
        Data_Utilizzo_Dwh=Sysdate 
      Where Lpad(Trim(Cod_Pdv),3,'0') = Substr(C_files.Nome_File,5,3)
      and data_vendita = to_char(to_date(substr(C_files.nome_file,9,6),'yymmdd'),'ddmmyyyy') ;
               

    --Creazione Di Una Mail Di Riepilogo (Da Attivare In Produzione)
    Pid := Null;
    V_Testo:='Caricati pdv: '|| V_Cod_Pdv||' nel file dati:'||C_Files.Nome_File ;
    Pid := Testo_Mail@Dcg_Prod(V_Testo, Pid);
    Send_Mail@Dcg_Prod('Terminazione caricamento scontrino SCN',Pid,Num_Gruppo_Dest);

      
  End If;
    
  commit; 
  
  Exception
    When no_data_found Then
      Dms_Com.Write_Jlt(V_Unit, 1, K_Error_Marker);
     DMS_COM.write_jlt(v_unit, 1, 'Warning!! pdv non trovato  ' || v_cod_pdv );
  end;

  End Loop;   
  
          
  end if;  -- termina il controllo iniziale sul caricamento della dw_stato       
 
  Exception
    When Others Then
      Dms_Com.Write_Jlt(V_Unit, 1, K_Error_Marker);
      Dms_Com.Write_Jlt(V_Unit, 1, 'Generata eccezione ' || Sqlcode || ' ' ||
                         Sqlerrm);
      Dms_Lat.Elab_Fail(Sqlerrm);
      Raise;
  End Dwh_Proc_Load_Scn_Amz;  

  /******************************************************************************
     NAME:       RicalcolaImportiCesti
     PURPOSE:    Funzione che effettua il ricalcolo degli importi dei cesti
                 prendendo i valori passati da 4store sui sui componenti

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        10/06/2020  M.Modena            prima versione
  *******************************************************************************/
 FUNCTION RicalcolaImportiCesti(v_pdv varchar2, v_date varchar2) RETURN VARCHAR2 IS

    v_unit                              VARCHAR2(30) := 'RicalcolaImportiCesti_____';
    k_error_marker             CONSTANT CHAR(22)     := '[*** !!! ***]  [ERROR]';
    v_return                            VARCHAR2(2)  := 'OK';
    v_cod_tipo_riga_cesto      CONSTANT INTEGER      := 28;
    v_cod_tipo_riga_articolo   CONSTANT INTEGER      := 0;
    v_cod_funzione             CONSTANT INTEGER      := 32;
    v_flg_elaborazione         CONSTANT CHAR(1)      := 'W';
    v_cod_componente           CONSTANT INTEGER      := 20;
    v_testo                             VARCHAR2(250);
    n_num_righe                         NUMBER;
    v_count                             NUMBER;

  BEGIN
    DMS_COM.write_jlt(v_unit, 1, 'Inizio ricalcolo importi cesti');
    
    /* cerchiamo tutti i cesti all'interno del set di righe sotto elaborazione per il pdv/data passati */
    for cesto in (select *
                    from gefa.JSON_SCN_RIGHE@dcg_prod t
                   where 1 = 1
                     and cod_tipo_riga                   = v_cod_tipo_riga_cesto --28,cesto
                     and cod_funzione                    in (32,65,66)--= v_cod_funzione        --32,vendita articolo o 66,annullo riga
                     and trim(upper(t.flg_elaborazione)) = v_flg_elaborazione    --'W',solo righe in stato Working
                     and lpad(t.COD_PDV, 4, '0')         = v_pdv                 --cod_pdv passato come parametro
                     and trim(t.dt_scontrino)            = v_date                --data stringa passata come parametro
                  ) loop
                  
        /* per ciascun cesto passato dal cursore modifichiamo gli importi con i valori presi da tutti i suoi componenti, diviso il numero di cesti uguali nello stesso scontrino */
        update gefa.JSON_SCN_RIGHE@dcg_prod d
           set prezzo_un = (select sum(prezzo_un)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select count(*)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_ecotassa = (select sum(im_ecotassa)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select count(*)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_netto_promo = (select sum(im_netto_promo)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select count(*)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_netto_promo_no_iva = (select sum(im_netto_promo_no_iva)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select count(*)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_iva = (select sum(im_iva)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select count(*)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_vendita_no_promo = (select sum(im_vendita_no_promo)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select count(*)
                              from gefa.JSON_SCN_RIGHE@dcg_prod r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )
         where 1 = 1
           and dt_scontrino         = cesto.dt_scontrino       --chiave cesto/scontrino
           and cod_insegna          = cesto.cod_insegna        --chiave cesto/scontrino
           and cod_pdv              = cesto.cod_pdv            --chiave cesto/scontrino
           and cod_cassa            = cesto.cod_cassa          --chiave cesto/scontrino
           and cod_scontrino        = cesto.cod_scontrino      --chiave cesto/scontrino
           and flg_elaborazione     = cesto.flg_elaborazione
           and cod_tipo_riga        = cesto.cod_tipo_riga      
           and cod_funzione         = cesto.cod_funzione       
           and ean_scanner          = cesto.cod_ean            --ean del cesto      
           ;
        
        update /*+ INDEX (d idx2_scn_righe) */ JSON_SCN_RIGHE_tmp d
           set prezzo_un = (select /*+ INDEX (r idx1_scn_righe) */ sum(prezzo_un)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select /*+ INDEX (r idx1_scn_righe) */ count(*)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_ecotassa = (select /*+ INDEX (r idx1_scn_righe) */ sum(im_ecotassa)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select /*+ INDEX (r idx1_scn_righe) */ count(*)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_netto_promo = (select /*+ INDEX (r idx1_scn_righe) */ sum(im_netto_promo)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select /*+ INDEX (r idx1_scn_righe) */ count(*)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_netto_promo_no_iva = (select /*+ INDEX (r idx1_scn_righe) */ sum(im_netto_promo_no_iva)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select /*+ INDEX (r idx1_scn_righe) */ count(*)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_iva = (select /*+ INDEX (r idx1_scn_righe) */ sum(im_iva)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select /*+ INDEX (r idx1_scn_righe) */ count(*)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             ),
               im_vendita_no_promo = (select /*+ INDEX (r idx1_scn_righe) */ sum(im_vendita_no_promo)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                               and cod_funzione         = d.cod_funzione
                               and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )/(select /*+ INDEX (r idx1_scn_righe) */ count(*)
                              from JSON_SCN_RIGHE_tmp r
                             where 1 = 1
                               and cod_tipo_riga        = v_cod_tipo_riga_cesto --articolo
                               and cod_funzione         = d.cod_funzione
                               and dt_scontrino         = d.dt_scontrino           --chiave cesto/scontrino
                               and cod_insegna          = d.cod_insegna            --chiave cesto/scontrino
                               and cod_pdv              = d.cod_pdv                --chiave cesto/scontrino
                               and cod_cassa            = d.cod_cassa              --chiave cesto/scontrino
                               and cod_scontrino        = d.cod_scontrino          --chiave cesto/scontrino
                               and ean_scanner          = d.cod_ean                --componenti con stesso ean del cesto
                             )
         where 1 = 1
           and dt_scontrino         = cesto.dt_scontrino       --chiave cesto/scontrino
           and cod_insegna          = cesto.cod_insegna        --chiave cesto/scontrino
           and cod_pdv              = cesto.cod_pdv            --chiave cesto/scontrino
           and cod_cassa            = cesto.cod_cassa          --chiave cesto/scontrino
           and cod_scontrino        = cesto.cod_scontrino      --chiave cesto/scontrino
           and flg_elaborazione     = cesto.flg_elaborazione
           and cod_tipo_riga        = cesto.cod_tipo_riga      
           and cod_funzione         = cesto.cod_funzione       
           and ean_scanner          = cesto.cod_ean            --ean del cesto      
           ;
           
        
        v_testo :=  ' data:'||to_char(cesto.dt_scontrino)||
                    '; pdv:'||to_char(cesto.cod_pdv)||
                  '; cassa:'||to_char(cesto.cod_cassa)||
              '; scontrino:'||to_char(cesto.cod_scontrino)||
                    '; ean:'||to_char(cesto.cod_ean);
                    
        DMS_COM.write_jlt(v_unit, 1, 'Modificati importi cesto: '||v_testo);   
           
        /* a questo punto dobbiamo aggiungere le ventilazioni di promozioni eventuali per farle comparire associate al cesto padre 
           perche' quelle che compaiono sulla tabella JSON_SCN_PROMO_JRET associate ai componenti le perdiamo */
           
        --select gefa.JSON_SCN_PROMO_JRET@dcg_prod
        --per tracciare se esiste gia' il record aggiuntivo
        select count(*) into n_num_righe
         from gefa.JSON_SCN_PROMO_JRET@dcg_prod
        where 1=1
          and dt_scontrino         = cesto.dt_scontrino       --chiave cesto/scontrino
          and cod_insegna          = cesto.cod_insegna        --chiave cesto/scontrino
          and cod_pdv              = cesto.cod_pdv            --chiave cesto/scontrino
          and cod_cassa            = cesto.cod_cassa          --chiave cesto/scontrino
          and cod_scontrino        = cesto.cod_scontrino      --chiave cesto/scontrino
          and flg_elaborazione     = cesto.flg_elaborazione
          and cod_riga_rif         = cesto.cod_riga ;
        
        --verifichiamo se esistono righe di promozione ventilata associate a righe dei componenti
        select count(*) into v_count
                from gefa.JSON_SCN_PROMO_JRET@dcg_prod r
               where 1 = 1
                 and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                 and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                 and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                 and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                 and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                 and flg_elaborazione     = cesto.flg_elaborazione
                 and cod_riga_rif in (select cod_riga
                                        from gefa.JSON_SCN_RIGHE@dcg_prod r
                                       where 1 = 1
                                         and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                                         and cod_funzione         = cesto.cod_funzione           --vendita articolo o annullo
                                         and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                                         and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                                         and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                                         and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                                         and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                                         and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                                         and ean_scanner          = cesto.cod_ean                --componenti con stesso ean del cesto)
               );
               
        --se non c'e' gia il record aggiuntivo e se ho le righe di ventilazione dei componenti
        --bisogna inserire il record aggiuntivo per il cesto padre       
        if(n_num_righe=0 and v_count>0) then
         INSERT INTO gefa.JSON_SCN_PROMO_JRET@dcg_prod
            (dt_scontrino       ,
             cod_insegna        , 
             cod_pdv            ,
             cod_cassa          ,
             cod_scontrino      ,
             cod_riga           ,
             cod_riga_rif       ,
             im_promo_ventilato ,
             im_rimanenza       ,
             dt_inserimento     ,
             flg_elaborazione   
             )
          VALUES
            (cesto.dt_scontrino       ,
             cesto.cod_insegna        , 
             cesto.cod_pdv            ,
             cesto.cod_cassa          ,
             cesto.cod_scontrino      ,
             (select distinct max(cod_riga)
                from gefa.JSON_SCN_PROMO_JRET@dcg_prod r
               where 1 = 1
                 and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                 and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                 and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                 and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                 and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                 and flg_elaborazione     = cesto.flg_elaborazione
                 and cod_riga_rif in (select cod_riga
                                        from gefa.JSON_SCN_RIGHE@dcg_prod r
                                       where 1 = 1
                                         and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                                         and cod_funzione         = cesto.cod_funzione           --vendita articolo o annullo
                                         and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                                         and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                                         and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                                         and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                                         and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                                         and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                                         and ean_scanner          = cesto.cod_ean                --componenti con stesso ean del cesto)
               ))           ,
             cesto.cod_riga       ,
             (select sum(im_promo_ventilato)
                from gefa.JSON_SCN_PROMO_JRET@dcg_prod r
               where 1 = 1
                 and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                 and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                 and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                 and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                 and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                 and flg_elaborazione     = cesto.flg_elaborazione
                 and cod_riga_rif in (select cod_riga
                                        from gefa.JSON_SCN_RIGHE@dcg_prod r
                                       where 1 = 1
                                         and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                                         and cod_funzione         = cesto.cod_funzione           --vendita articolo o annullo
                                         and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                                         and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                                         and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                                         and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                                         and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                                         and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                                         and ean_scanner          = cesto.cod_ean                --componenti con stesso ean del cesto)
               )) ,
             (select sum(im_rimanenza)
                from gefa.JSON_SCN_PROMO_JRET@dcg_prod r
               where 1 = 1
                 and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                 and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                 and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                 and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                 and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                 and flg_elaborazione     = cesto.flg_elaborazione
                 and cod_riga_rif in (select cod_riga
                                        from gefa.JSON_SCN_RIGHE@dcg_prod r
                                       where 1 = 1
                                         and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                                         and cod_funzione         = cesto.cod_funzione           --vendita articolo o annullo
                                         and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                                         and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                                         and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                                         and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                                         and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                                         and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                                         and ean_scanner          = cesto.cod_ean                --componenti con stesso ean del cesto)
               ))       ,
             sysdate              ,
             cesto.flg_elaborazione   );
         
         INSERT INTO JSON_SCN_PROMO_JRET_tmp
            (dt_scontrino       ,
             cod_insegna        , 
             cod_pdv            ,
             cod_cassa          ,
             cod_scontrino      ,
             cod_riga           ,
             cod_riga_rif       ,
             im_promo_ventilato ,
             im_rimanenza       ,
             dt_inserimento     ,
             flg_elaborazione   
             )
          VALUES
            (cesto.dt_scontrino       ,
             cesto.cod_insegna        , 
             cesto.cod_pdv            ,
             cesto.cod_cassa          ,
             cesto.cod_scontrino      ,
             (select distinct max(cod_riga)
                from JSON_SCN_PROMO_JRET_tmp r
               where 1 = 1
                 and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                 and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                 and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                 and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                 and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                 and flg_elaborazione     = cesto.flg_elaborazione
                 and cod_riga_rif in (select cod_riga
                                        from JSON_SCN_RIGHE_tmp r
                                       where 1 = 1
                                         and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                                         and cod_funzione         = cesto.cod_funzione           --vendita articolo o annullo
                                         and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                                         and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                                         and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                                         and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                                         and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                                         and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                                         and ean_scanner          = cesto.cod_ean                --componenti con stesso ean del cesto)
               ))           ,
             cesto.cod_riga       ,
             (select sum(im_promo_ventilato)
                from JSON_SCN_PROMO_JRET_tmp r
               where 1 = 1
                 and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                 and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                 and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                 and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                 and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                 and flg_elaborazione     = cesto.flg_elaborazione
                 and cod_riga_rif in (select cod_riga
                                        from JSON_SCN_RIGHE_tmp r
                                       where 1 = 1
                                         and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                                         and cod_funzione         = cesto.cod_funzione           --vendita articolo o annullo
                                         and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                                         and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                                         and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                                         and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                                         and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                                         and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                                         and ean_scanner          = cesto.cod_ean                --componenti con stesso ean del cesto)
               )) ,
             (select sum(im_rimanenza)
                from JSON_SCN_PROMO_JRET_tmp r
               where 1 = 1
                 and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                 and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                 and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                 and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                 and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                 and flg_elaborazione     = cesto.flg_elaborazione
                 and cod_riga_rif in (select cod_riga
                                        from JSON_SCN_RIGHE_tmp r
                                       where 1 = 1
                                         and cod_tipo_riga        = v_cod_tipo_riga_articolo --articolo
                                         and cod_funzione         = cesto.cod_funzione           --vendita articolo o annullo
                                         and cod_sorgente_lettura = v_cod_componente         --componente di un cesto
                                         and dt_scontrino         = cesto.dt_scontrino           --chiave cesto/scontrino
                                         and cod_insegna          = cesto.cod_insegna            --chiave cesto/scontrino
                                         and cod_pdv              = cesto.cod_pdv                --chiave cesto/scontrino
                                         and cod_cassa            = cesto.cod_cassa              --chiave cesto/scontrino
                                         and cod_scontrino        = cesto.cod_scontrino          --chiave cesto/scontrino
                                         and ean_scanner          = cesto.cod_ean                --componenti con stesso ean del cesto)
               ))       ,
             sysdate              ,
             cesto.flg_elaborazione   );
          
           n_num_righe := SQL%rowcount;
           DMS_COM.write_jlt(v_unit, 1,'gefa.JSON_SCN_PROMO_JRET: record insert: '|| n_num_righe);
        elsif(n_num_righe>0) then
           DMS_COM.write_jlt(v_unit, 1,'gefa.JSON_SCN_PROMO_JRET: record presenti: '|| n_num_righe);
        end if;
    end loop; 
       
    COMMIT;

    DMS_COM.write_jlt(v_unit, 1, 'Fine ricalcolo importi cesti: '||v_return);

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END RicalcolaImportiCesti;

  /******************************************************************************
     NAME:       MailRiepilogo4S
     PURPOSE:    Funzione che effettua la generazione di una mail di riepilogo
                 sul caricamento degli scontrini ricevuti da 4STORE

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        01/07/2020  M.Modena            prima versione
  *******************************************************************************/
 FUNCTION MailRiepilogo4S(v_date date) RETURN VARCHAR2 IS

    v_unit                              VARCHAR2(30)   := 'MailRiepilogo4S**********';
    k_error_marker             CONSTANT CHAR(22)       := '[*** !!! ***]  [ERROR]';
    v_return                            VARCHAR2(2)    := 'OK';
    n_num_righe                         NUMBER;
    v_count                             NUMBER;
    PID                                 NUMBER;
    NUM_GRUPPO_DEST                     VARCHAR2(10)   := '1011';
    v_testo                             VARCHAR2(4000) := '';
    vcName                              VARCHAR2(255);
    fHandle                             UTL_FILE.FILE_TYPE;

  BEGIN
    DMS_COM.write_jlt(v_unit, 1, 'Inizio mail di riepilogo');
    --nome file da allegare
    vcName:='Riepilogo4STORE_'|| to_char(SYSDATE, 'yyyymmdd_hh24miss')||'.csv';
    
    PID := null;
    
    --impostiamo un reference al file di testo dove scriveremo
    fHandle := utl_file.fopen('INC_GIOR_MAIL_DIR', vcName, 'w'); --in write mode...
    --scrive la testata nel file
    utl_file.put_line(fHandle,
      'Data;PDV;Cassa;Cod Stato;Stato Scontrino;Cod Tipo;Tipo Scontrino;Caricato su Oracle;Totale per PDV Cassa e Stato;Totale_per PDV/Tipo;Totale per Tipo'
      );
      
    -- prima riga intestazione con i titoli degli header
    Select 
              LPAD( 'Data'                         ,10,' ')  ||'   '||
              LPAD( 'PDV'                          ,4,' ')   ||'   '||
              LPAD( 'Cassa'                        ,5,' ')   ||'   '||
              LPAD( 'Cod Stato'                    ,9,' ')   ||'   '||
              LPAD( 'Stato Scontrino'              ,20,' ')  ||'   '||
              LPAD( 'Cod Tipo'                     ,8,' ')   ||'   '||
              LPAD( 'Tipo Scontrino'               ,20,' ')  ||'   '||
              LPAD( 'Caricato su Oracle'           ,18,' ')  ||'   '||
              LPAD( 'Totale per PDV Cassa e Stato' ,28,' ')  ||'   '||
              LPAD( 'Totale_per PDV/Tipo'          ,19,' ')  ||'   '||
              LPAD( 'Totale per Tipo'              ,15,' ')
      into v_testo
      from dual;
    PID := TESTO_MAIL@dcg_prod(v_testo, PID);
    -- seconda riga intestazione con i separatori degli header  
    Select 
              RPAD( '-' ,10,'-')  ||'   '||
              RPAD( '-' ,4,'-')   ||'   '||
              RPAD( '-' ,5,'-')   ||'   '||
              RPAD( '-' ,9,'-')   ||'   '||
              RPAD( '-' ,20,'-')  ||'   '||
              RPAD( '-' ,8,'-')   ||'   '||
              RPAD( '-' ,20,'-')  ||'   '||
              RPAD( '-' ,18,'-')  ||'   '||
              RPAD( '-' ,28,'-')  ||'   '||
              RPAD( '-' ,19,'-')  ||'   '||
              RPAD( '-' ,15,'-')
      into v_testo
      from dual;
    PID := TESTO_MAIL@dcg_prod(v_testo, PID);
      DMS_COM.write_jlt(v_unit, 1, 'Inizio query1');
    --generazione delle righe del report  
    FOR cur IN (select ---record del file: campi tutti attaccati separati da ';'
                      (
                        trim(data_scontrino)||';'||
                        trim(pdv)||';'||
                        trim(cassa)||';'||
                        trim(cod_stato)||';'||
                        trim(stato_scontrino)||';'||
                        trim(cod_tipo)||';'||
                        trim(tipo_scontrino)||';'||
                        trim(caricato_su_oracle)||';'||
                        trim(totale_per_pdv_cassa_e_stato)||';'||
                        trim(totale_per_pdv_tipo)||';'||
                        trim(totale_per_tipo)
                      ) recfile,
                       (
                        LPAD(data_scontrino               ,10,' ')  ||'   '||
                        LPAD(pdv                          ,4,' ')   ||'   '||
                        LPAD(cassa                        ,5,' ')   ||'   '||
                        LPAD(cod_stato                    ,9,' ')   ||'   '||
                        LPAD(stato_scontrino              ,20,' ')  ||'   '||
                        LPAD(cod_tipo                     ,8,' ')   ||'   '||
                        LPAD(tipo_scontrino               ,20,' ')  ||'   '||
                        LPAD(caricato_su_oracle           ,18,' ')  ||'   '||
                        LPAD(totale_per_pdv_cassa_e_stato ,28,' ')  ||'   '||
                        LPAD(totale_per_pdv_tipo          ,19,' ')  ||'   '||
                        LPAD(totale_per_tipo              ,15,' ')
                       )                                                      as recmail
                 from (
                        select distinct 
                               dt_scontrino      as data_scontrino,
                               cod_pdv           as pdv,
                               cod_cassa         as cassa,
                               cod_stato_scn     as cod_stato,
                               p1.alias          as stato_scontrino,
                               cod_tipo_scn      as cod_tipo,
                               p2.alias          as tipo_scontrino,
                               p1.attivo         as caricato_su_oracle,
                               count(distinct cod_scontrino) over (partition by dt_scontrino,cod_pdv,cod_cassa,cod_stato_scn,cod_tipo_scn,p1.alias,p1.attivo,p2.alias) as totale_per_pdv_cassa_e_stato,
                               count(cod_scontrino) over (partition by p1.attivo,cod_tipo_scn,cod_pdv,dt_scontrino) as totale_per_pdv_tipo,
                               count(cod_scontrino) over (partition by p1.attivo,cod_tipo_scn,dt_scontrino) as totale_per_tipo 
                          from gefa.JSON_SCN_TESTATA@dcg_prod f,
                               (select * from PROC_4S_PARAMS where parametro in ('COD_STATO_SCN')) p1,
                               (select * from PROC_4S_PARAMS where parametro in ('COD_TIPO_SCN')) p2
                         where 1 = 1
                           and p1.valore = cod_stato_scn
                           and p2.valore = cod_tipo_scn
                           and (/*f.dt_inserimento >= trunc(sysdate - 1) or */f.flg_elaborazione = 'W')
                           and to_date(trim(f.dt_scontrino), 'yyyy-mm-dd') < trunc(sysdate)
                         order by dt_scontrino desc,p1.attivo desc, cod_stato_scn, cod_tipo_scn, cod_pdv, cod_cassa)
                 ) LOOP
      
              --scrive la riga n-esima nel file
              utl_file.put_line(fHandle,cur.recfile);
              --scrive la riga della mail
              v_testo := cur.recmail;
              PID     := testo_mail@dcg_prod(v_testo, PID);
    END LOOP;
    
    --scrive la riga n-esima nel file
    utl_file.put_line(fHandle,';');
    --scrive la riga n-esima nella mail
    PID     := testo_mail@dcg_prod('', PID);
    
    DMS_COM.write_jlt(v_unit, 1, 'Inizio query2');
    
    FOR cur IN (select (
                        trim(data_scontrino)||';'||
                        trim(pdv)||';'||
                        trim(cassa)||';'||
                        trim(cod_stato)||';'||
                        trim(stato_scontrino)||';'||
                        trim(cod_tipo)||';'||
                        trim(tipo_scontrino)||';'||
                        trim(caricato_su_oracle)||';'||
                        trim(totale_per_pdv_cassa_e_stato)||';'||
                        trim(totale_per_pdv_tipo)||';'||
                        trim(totale_per_tipo)                    
                      ) recfile,
                      (
                        LPAD(data_scontrino               ,10,' ')  ||'   '||
                        LPAD(pdv                          ,4,' ')   ||'   '||
                        LPAD(cassa                        ,5,' ')   ||'   '||
                        LPAD(cod_stato                    ,9,' ')   ||'   '||
                        LPAD(stato_scontrino              ,20,' ')  ||'   '||
                        LPAD(cod_tipo                     ,8,' ')   ||'   '||
                        LPAD(tipo_scontrino               ,20,' ')  ||'   '||
                        LPAD(caricato_su_oracle           ,18,' ')  ||'   '||
                        LPAD(totale_per_pdv_cassa_e_stato ,28,' ')  ||'   '||
                        LPAD(totale_per_pdv_tipo          ,19,' ')  ||'   '||
                        LPAD(totale_per_tipo              ,15,' ')
                       )                                                      as recmail
                 from (
                        select distinct 
                               dt_vendita        as data_scontrino,
                               f.cod_pdv         as pdv,
                               f.cod_cassa       as cassa,
                               '-'               as cod_stato,
                               'chiuso'          as stato_scontrino,
                               '-'               as cod_tipo,
                               'fine giorno'     as tipo_scontrino,
                               decode(f.flg_elaborazione,'F','Y','N')       as caricato_su_oracle,
                               count(*) over (partition by dt_vendita,f.cod_pdv,f.cod_cassa) as totale_per_pdv_cassa_e_stato,
                               count(*) over (partition by f.cod_pdv,dt_vendita) as totale_per_pdv_tipo,
                               count(*) over (partition by dt_vendita) as totale_per_tipo --select *
                          from gefa.JSON_FC_DETT_EOD_JRET@dcg_prod f,
                               GEFA.JSON_FC_DETT_EOD@dcg_prod t
                         where 1 = 1
                           and t.cod_finegiorno=f.cod_finegiorno
                           and t.cod_pdv=f.cod_pdv
                           and t.cod_cassa=f.cod_cassa
                           and to_date(dt_vendita,'yyyy-mm-dd') >= trunc(sysdate - 1)
                           --and f.flg_elaborazione='F'
                         order by dt_vendita desc, f.cod_pdv, f.cod_cassa)
                 ) LOOP
      
              --scrive la riga n-esima nel file
              utl_file.put_line(fHandle,cur.recfile);
              --scrive la riga della mail
              v_testo := cur.recmail;
              PID     := testo_mail@dcg_prod(v_testo, PID);
              
    END LOOP;
    
    
    utl_file.fflush(fHandle); --effettua il flushing del buffer
    utl_file.fclose(fHandle); --chiudiamo il reference al file

     DMS_COM.write_jlt(v_unit,1,'Scritto file '||vcName);
    
    -----send_mail@dcg_prod('Report di caricamento scontrini da 4STORE per il '||to_char(sysdate,'dd/mm/yyyy'),PID,NUM_GRUPPO_DEST);
    send_mail_1att@dcg_prod('Report di caricamento scontrini da 4STORE per il '||to_char(sysdate,'dd/mm/yyyy'),PID,NUM_GRUPPO_DEST,vcName,vcName,'INC_GIOR_MAIL_DIR','application/vnd.ms-excel');
    
    DMS_COM.write_jlt(v_unit, 1, 'Fine mail di ripeilogo: '||v_return);

    RETURN v_return;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 'KO';
  END MailRiepilogo4S;

 /******************************************************************************
     NAME:       DWH_PROC_LOAD_SCN_4S
     PURPOSE:    Procedura che carica gli scontrini da 4S

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        07/10/2019  N Delle Donne       
     1.2        11/06/2020  M.Modena            chiamata a RicalcolaImportiCesti
  *******************************************************************************/  
  PROCEDURE DWH_PROC_LOAD_SCN_4S(p_job    IN VARCHAR2,
                              p_period IN VARCHAR2,
                              p3       IN VARCHAR2 DEFAULT NULL,
                              p4       IN VARCHAR2 DEFAULT NULL,
                              p5       IN VARCHAR2 DEFAULT NULL,
                              p6       IN VARCHAR2 DEFAULT NULL,
                              p7       IN VARCHAR2 DEFAULT NULL,
                              p8       IN VARCHAR2 DEFAULT NULL,
                              p9       IN VARCHAR2 DEFAULT NULL,
                              p10      IN VARCHAR2 DEFAULT NULL,
                              p11      IN VARCHAR2 DEFAULT NULL
  ) IS
    v_unit                  VARCHAR2(30) := 'DWH_PROC_LOAD_SCN_4S';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe             NUMBER:=0;
    r_env                   dms_com_cfg_environment%ROWTYPE;
    v_command               VARCHAR2(1000);
    v_DAT_PATH              VARCHAR2(255);
    v_DAT_LISTA_FILE        VARCHAR2(100);
    v_FILE_NAME_LISTA       VARCHAR2(100);
    v_FILE_NAME_SETTIMANA   VARCHAR2(100);
    v_CTL_FILE              VARCHAR2(100);
    v_File_Name             VARCHAR2(10); --si usa in proc per nome file
    v_data                  VARCHAR2(8);
    v_Nome_File_bck         VARCHAR2(20);
    v_return                NUMBER;
    v_settimana             INTEGER;
    v_sett_corrente         INTEGER;
    v_anno                  INTEGER;
    v_anno_loop             INTEGER;
    v_prosegui              NUMBER := 0;
    v_flusso                VARCHAR2(20);
    v_codifica              VARCHAR2(20);
    v_code_repm             VARCHAR2(20);
    v_code_conto            VARCHAR2(20);
    v_code_entity           VARCHAR2(20);
    v_FileHandle            utl_file.file_type;
    v_NewLine               VARCHAR2(200);
    v_cod_pdv               VARCHAR2(4);
    --variabili per creazione ed invio MAIL
    v_num_scarti            NUMBER := 0;
    PID                     NUMBER;
    NUM_GRUPPO_DEST         VARCHAR2(10) := '14';   -- 779 x test  14 per produzione
    v_testo                 VARCHAR2(4000);
    crlf          CONSTANT  VARCHAR2(2) := CHR(13) || CHR(10);
    v_cod_transazione       number :=0;
    v_conta_4s_pdvdt        number :=0;            --conta i pdv\dt_competanze arrivati da 4S e gia in dw_stato
    v_upd_json              VARCHAR2(4000);        --usato per cursore che esegue update uguali sulle tab json
    v_dt_scontrino_cur      VARCHAR2(100):='init';
    v_cod_insegna_cur       VARCHAR2(100):='init';
    v_cod_pdv_cur           VARCHAR2(100):='init';
    v_cod_cassa_cur         VARCHAR2(100):='init';
    v_cod_scontrino_cur     VARCHAR2(100):='init';
    v_dt_inserimento_cur    VARCHAR2(100):='init';
    v_nrec                  number:=0;
    v_res                   VARCHAR2(2):='OK';
    
    v_return4               NUMBER; --exit code stream C&C
    
    -----gli update sono tutti uguali
    -----la tabella PROC_TMP_4S_UT (utility) contiene tabella, dblink, e filtri aggiuntivi
    -----il cursore genera un loop che crea automaticamente l'update
    CURSOR UT_4S is
    select DISTINCT
     trim(TABJSON)    as TABJSON,
     trim(DBLINK)     as DBLINK,
     trim(FILTRI_AGG) as FILTRI_AGG
    from proc_tmp_4s_ut
    ;
    
  BEGIN
    
    Execute Immediate 'truncate table PROC_TMP_EAN';
    Dms_Com.Write_Jlt(V_Unit, 1, 'truncate table PROC_TMP_EAN');
    
    Insert Into Proc_Tmp_Ean
      Select Ean_Id, Art_Id, '' From ANAG.EAN;
    
    V_Nrec:=Sql%Rowcount;
    Commit;
    
    If V_Nrec = 0 Then
       Dms_Com.Write_Jlt(V_Unit, 1, 'Warning!!! Nessun record inserito in PROC_TMP_EAN:');
    Else
       Dms_Com.Write_Jlt(V_Unit, 1, 'Ok: Record inseriti in PROC_TMP_EAN:'||V_Nrec);
    End If ;
  
  /***************************************************************************************************************
   ***************************************************************************************************************
   *         PRIMO PASSO - sincronizzazione delle sorgenti caricate dai documenti json di 4store
   ***************************************************************************************************************
   ***************************************************************************************************************/
   
     
    --impostiamo un riferimento temporale univoco su cui basare tutte le sincronizzazioni.
    --select sysdate+1 into v_sysdate from dual;
    select sysdate into v_sysdate from dual;
    DMS_COM.write_jlt(v_unit, 1, 'Impostazione riferimento temporale: '||to_char(v_sysdate,'dd/mm/yyyy hh24:mi:ss'));
    
    --sincronizziamo le sorgenti impostando in stato Working i record nuovi arrivati finora. Se ne arrivano successivamente non vengono considerat
    DMS_COM.write_jlt(v_unit, 1, 'Inizio sincronizzazione ''Working'' sorgenti');
    update gefa.JSON_SCN_TESTATA@dcg_prod            f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate;  n_num_righe := SQL%rowcount;
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_TESTATA');
    update gefa.JSON_SCN_TESTATA_JRET@dcg_prod       f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate;  
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_TESTATA_JRET');
    update gefa.JSON_SCN_RIGHE@dcg_prod              f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_RIGHE');
    update /*+index(f IDX3_RIGHE_RET) */ gefa.JSON_SCN_RIGHE_JRET@dcg_prod         f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_RIGHE_JRET');
    update gefa.JSON_SCN_FP@dcg_prod                 f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_FP');
    update gefa.JSON_SCN_FP_JRET@dcg_prod            f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_FP_JRET');
    update gefa.JSON_SCN_IVA@dcg_prod                f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_IVA');
    update gefa.JSON_SCN_TESTATA_CARD@dcg_prod       f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_TESTATA_CARD');
    update gefa.JSON_SCN_TESTATA_CARD_JRET@dcg_prod  f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_TESTATA_CARD_JRET');
    update gefa.JSON_SCN_DETT_CARD@dcg_prod          f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_DETT_CARD');
    update gefa.JSON_SCN_PROMO@dcg_prod              f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_PROMO');
    update gefa.JSON_SCN_PROMO_JRET@dcg_prod         f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_PROMO_JRET');
    update gefa.JSON_SCN_ART_VIRTUALI@dcg_prod       f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_ART_VIRTUALI');
    update gefa.JSON_SCN_ART_VIRTUALI_JRET@dcg_prod  f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_ART_VIRTUALI_JRET');
    update gefa.JSON_SCN_AUTORIZZ_SV@dcg_prod        f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_AUTORIZZ_SV');
    update gefa.JSON_SCN_EAN_NON_CASSA@dcg_prod      f set f.flg_elaborazione='W' where f.flg_elaborazione='N' and to_date(trim(f.dt_scontrino),'yyyy-mm-dd')<trunc(v_sysdate) and DT_INSERIMENTO <= v_sysdate; 
    DMS_COM.write_jlt(v_unit, 1, 'JSON_SCN_EAN_NON_CASSA');
    commit;
    DMS_COM.write_jlt(v_unit, 1, 'Effettuata sincronizzazione ''Working'' '||n_num_righe||' scontrini sulle sorgenti');
   
    --marchiamo con 'A' i pdv che non rientrano nella gestione json
    DMS_COM.write_jlt(v_unit, 1, 'Inizio sincronizzazione ''Anomalia'' sorgenti con pdv non in anagrafica');
    update gefa.JSON_SCN_TESTATA@dcg_prod            f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';  n_num_righe := SQL%rowcount;
    update gefa.JSON_SCN_TESTATA_JRET@dcg_prod       f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';  
    update gefa.JSON_SCN_RIGHE@dcg_prod              f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update /*+index(f IDX1_RIGHE_JRET) */ gefa.JSON_SCN_RIGHE_JRET@dcg_prod         f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_FP@dcg_prod                 f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_FP_JRET@dcg_prod            f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_IVA@dcg_prod                f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_TESTATA_CARD@dcg_prod       f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_TESTATA_CARD_JRET@dcg_prod  f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_DETT_CARD@dcg_prod          f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_PROMO@dcg_prod              f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_PROMO_JRET@dcg_prod         f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_ART_VIRTUALI@dcg_prod       f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_ART_VIRTUALI_JRET@dcg_prod  f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_AUTORIZZ_SV@dcg_prod        f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    update gefa.JSON_SCN_EAN_NON_CASSA@dcg_prod      f set f.flg_elaborazione='A' where 'E_'||case when length(to_char(f.COD_PDV))=4 then to_char(f.COD_PDV) else lpad(f.COD_PDV,3,'0') end not in (select cd_entity from etl.dcg_an_pdv@dcg_prod where FLG_4S='Y' and to_date(f.dt_scontrino,'yyyy-mm-dd') >= DT_AVVIO_4S) and f.flg_elaborazione='W';
    commit;
    DMS_COM.write_jlt(v_unit, 1, 'Effettuata sincronizzazione ''Anomalia'' '||n_num_righe||' scontrini sulle sorgenti con pdv non in anagrafica');

    BEGIN
      v_return4:=disableScnProva();
      if v_return4=0 then 
        DMS_COM.write_jlt(v_unit, 1, 'Errore sincronizzazione ''Anomalia'' scontrini sulle sorgenti con pdv non in anagrafica');
      end if;
    EXCEPTION
      WHEN OTHERS THEN
        DMS_COM.write_jlt(v_unit, 1, 'Eccezione sincronizzazione ''Anomalia'' scontrini sulle sorgenti con pdv non in anagrafica');
    END;
    
  /***************************************************************************************************************
   ***************************************************************************************************************
   *         SECONDO PASSO - preparo lista PDV\DtComp da 4S attesi, arrivati e non (solo per ieri e oggi)
   ***************************************************************************************************************
   ***************************************************************************************************************/
    DMS_COM.write_jlt(v_unit, 1,' ');
    DMS_COM.write_jlt(v_unit, 1,'------ gestione blocchi attesi, arrivati e non pervenuti ------');

    /******************************************************************************
    CREATA TABELLA DI APPOGGIO PROC_TMP_4S_PDVDT
    che contiene in truncate\insert tutti i pdv/data competenza scaricati dalla testata
    valorizzando il campo 'PRESENTE' prima a N, poi a Y per i pdv\dt_competenza che sono gia' nella dw_stato e andranno RICARICATI
    e SEGNALATI (vedi vedi minuta incontro del 20191017) --> se arrivano due volte gli stessi dati si vuole la sovrascrittura di quelli precedenti

    -- in realt? se i doppioni arrivano troppi giorni (quanti? --> v_cutoff=3) dopo il primo invio e' da considerarsi anomalo e va segnalato
  
    caricare la anag.dw_stato per i pdv\data arrivati da 4S
    le regole sono:
    1)tutti i PDV\Data competenza scaricati dalla testata : dovevano arrivare da 4S (segregazione) e sono arrivati
    ---->insert\update su ANAG.DW_STATO dalla PROC_TMP_4S_PDVDT
    ---->se ci sono aggiorno DATA_LOAD_DB_PROC =null, DATA_RICEZIONE=sysdate  (riprocessa il pdv\data)
    ---->li segnalo e faccio il recupero (vedi sotto) , entrano in PROC_TEMP_FILE e se va tutto bene avranno DATA_LOAD_DB_PRO valorizzata
    ---->se non ci sono inserisco il record con DATA_LOAD_DB_PROC =null, DATA_RICEZIONE=sysdate, TIPO_FILE=SCN,
    --
    2)pdv con 4S (dcg.an_pdv.flg_4s=Y), ma senza dati sulle tabelle JSON con dt_competenza sysdate e sysdate-1
    ---->inserire record in ANAG.DW_STATO: DATA_LOAD_DB_PROC =null, DATA_RICEZIONE=sysdate, TIPO_FILE=SCN,
    ---->questi NON entreranno nelle PROC_TEMP_FILE e non saranno elaborati, non avranno  DATA_LOAD_DB_PROC valorizzata a fine caricamenteo
    ---->da qui si fa mail di controllo 'pdv doveva invaire scontrini da 4S e non lo ha fatto' in procedura consolida

    caricare la tabella PROC_TMP_FILE
    NOME_FILE VARCHAR2(14) SCN + PDV (3 cifre con 0 in testa) + . + data competenza (formato YYMMDD)
    CARICATO CHAR(1) valorizzato inizialmente ad N, appena i dati del PDV\Data competenza viene caricato nella DWH_DATI_SCN_STOR viene valorizzato a Y
    i PDV che hanno effettivamente inviato dati per le varie dt_competenza
    quindi legge dalla PROC_TMP_4S_PDVDT
    Il nome_file contiene pdv e dt_competenza
    SEGUE LE STESSE LOGICHE IN ESSERE OGGI: utilizza filtro_negozio
    ******************************************************************************/

    -------tronco e ripopolo la PROC_TMP_4S_PDVDT
    EXECUTE IMMEDIATE 'truncate table PROC_TMP_4S_PDVDT';
    DMS_COM.write_jlt(v_unit, 1,'truncate table PROC_TMP_4S_PDVDT: eseguito');
    INSERT INTO PROC_TMP_4S_PDVDT (NEG_ID,DATA_FILE,PRESENTE)
    (
     select DISTINCT
      case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) else lpad(t.COD_PDV,3,'0') end                      as neg_id,
      to_date(trim(t.dt_scontrino),'yyyy-mm-dd')                                                                         as data_file,
      'N'                                                                                                                as PRESENTE
        ------->>>>modifica per SVILUPPO
     from gefa.json_scn_testata@dcg_prod t
     where t.flg_elaborazione='W'
     and trim(t.COD_TIPO_SCN)  in (select to_number(valore) from etl.proc_4s_params where modulo='DWH_PROC_LOAD_SCN_4S' and parametro='COD_TIPO_SCN'  and attivo='Y') 
     and trim(t.COD_STATO_SCN) in (select to_number(valore) from etl.proc_4s_params where modulo='DWH_PROC_LOAD_SCN_4S' and parametro='COD_STATO_SCN' and attivo='Y')
    )
    ;
    n_num_righe := SQL%rowcount;
    COMMIT;
    DMS_COM.write_jlt(v_unit, 1,'PROC_TMP_4S_PDVDT: Blocchi pdv/data attesi e arrivati da 4S, record caricati: '|| n_num_righe);
    
    -------identifico pdv\dt_competenza gia' in dw_stato: serve per RICARICO
    n_num_righe:=0;
    for c in (select * from anag.dw_stato D where data_file in (select distinct data_file from PROC_TMP_4S_PDVDT))
    loop  
      UPDATE PROC_TMP_4S_PDVDT S
      SET PRESENTE = 'Y'
      where c.neg_id = S.neg_id 
        and c.data_file = S.data_file 
        and c.tipo_file = 'SCN';
        
        n_num_righe := n_num_righe + SQL%rowcount;
    end loop;
    COMMIT;
    DMS_COM.write_jlt(v_unit, 1,'PROC_TMP_4S_PDVDT: Blocchi pdv/data esistenti in dw_stato: '|| n_num_righe);
    --marchiamo come presente='X' i recuperi fuori dal cutoff
    UPDATE PROC_TMP_4S_PDVDT S
    SET PRESENTE = 'X'
    where data_file<=trunc(v_sysdate)-v_cutoff;
    
    n_num_righe := SQL%rowcount;
    COMMIT;
    DMS_COM.write_jlt(v_unit, 1,'PROC_TMP_4S_PDVDT: scontrini anomali in dw_stato: '|| n_num_righe);


    -------ANAG.DW_STATO: insert\update PDV\Date effettivamente arrivati da 4S
    MERGE INTO anag.dw_stato D
    USING (select neg_id, 
                  data_file, 
                  'SCN' tipo_file 
             from proc_tmp_4s_pdvdt
             where presente!='X' or presente is null) S
    ON (D.neg_id = S.neg_id and D.data_file = S.data_file and D.tipo_file = S.tipo_file)
    WHEN MATCHED THEN
      UPDATE SET D.DATA_RICEZIONE = sysdate, D.DATA_LOAD_DB_PROC = null
    WHEN NOT MATCHED THEN
      INSERT
        (D.neg_id,
         D.data_file,
         D.tipo_file,
         D.DATA_RICEZIONE,
         D.DATA_LOAD_DB_PROC)
      VALUES
        (S.neg_id, S.data_file, S.tipo_file, sysdate, null);
    n_num_righe := SQL%rowcount;
    COMMIT;
    DMS_COM.write_jlt(v_unit, 1,'ANAG.DW_STATO: record insert\update nella: '|| n_num_righe);

    -------ANAG.DW_STATO: inserisco PDV\Date ieri SOLO di pdv che dovevano mandare
    -------e non hanno mandato dati da 4S: faccio SOLO insert qaundo non c'?' gia'
    -------MERGE Con solo WHEN NOT MATCHED THEN INSERT
    MERGE INTO anag.dw_stato D
    USING
     (
      select neg_id,
             data_file,
             tipo_file
      from
           ------->>>>modifica per SVILUPPO
       (select DISTINCT substr(anpdv.cd_entity,3) neg_id ,
                        'SCN'                     tipo_file 
          from etl.dcg_an_pdv@dcg_prod anpdv 
         where anpdv.FLG_4S='Y' and trunc(sysdate-1)>=anpdv.DT_AVVIO_4S) pdv,
       (select data_file from (select trunc(sysdate-1) data_file from dual)) dt
     ) S
    ON (D.neg_id = S.neg_id and D.data_file = S.data_file and D.tipo_file = S.tipo_file)
    WHEN NOT MATCHED THEN
      INSERT (D.neg_id, D.data_file, D.tipo_file, D.DATA_RICEZIONE,D.DATA_LOAD_DB_PROC)
      VALUES (S.neg_id, S.data_file, S.tipo_file, sysdate         , null              )
    ;
    n_num_righe := SQL%rowcount;
    COMMIT;
    DMS_COM.write_jlt(v_unit, 1,'ANAG.DW_STATO: PDV da 4S senza record su JSON per competenza di ieri, inseriti: '|| n_num_righe);

    -------PROC_TMP_FILE:
    -------insert PDV\Date effettivamente arrivati da 4S
    -------PER RILANCIO IN CASO DI ERRORE: prima di inserire , cancello

    -------delete
    DELETE PROC_TMP_FILE
    WHERE NOME_FILE in
    (
     SELECT   'SCN'||lpad(F.neg_id,4,'0')||'.'||to_char(D.data_file,'RRmmdd') as NOME_FILE
     FROM   filtro_negozio F,
            dw_stato D,
            ---aggiunta tab pdv\dt_c. effettivamente arrivate da JSON
            PROC_TMP_4S_PDVDT PDVDT_4S
     WHERE (1=1)
     ---aggiunta tab pdv\dt_c. effettivamente arrivate da JSON
     and  lpad(PDVDT_4S.neg_id,4,'0') = lpad(D.neg_id,4,'0')
     and  PDVDT_4S.data_file=D.data_file
     and  (PDVDT_4S.presente!='X' or PDVDT_4S.presente is null)
     ---filtri e legami essitenti
     and  F.filtro_id = 'P'
     AND  D.Tipo_file = decode('P','R','REP','SCN')
     AND  lpad(F.neg_id,4,'0') = lpad(D.neg_id,4,'0')
     AND  decode('P',  'T',data_load_db_test,
         'D',data_load_db_diag,
         'P',data_load_db_proc,
         'R',data_load_db_proc,
         sysdate) is null
    )
    ;
    n_num_righe := SQL%rowcount;
    COMMIT;
    DMS_COM.write_jlt(v_unit, 1,'PROC_TMP_FILE: Blocchi pdv/data cancellati: '|| n_num_righe);

    -------insert
    INSERT INTO PROC_TMP_FILE
    SELECT   'SCN'||lpad(F.neg_id,4,'0')||'.'||to_char(D.data_file,'RRmmdd'), 'N'
    FROM   filtro_negozio F,
           dw_stato D,
           ---aggiunta tab pdv\dt_c. effettivamente arrivate da JSON
           PROC_TMP_4S_PDVDT PDVDT_4S
    WHERE (1=1)
    ---aggiunta tab pdv\dt_c. effettivamente arrivate da JSON
    and  lpad(PDVDT_4S.neg_id,4,'0') = lpad(D.neg_id,4,'0')
    and  PDVDT_4S.data_file=D.data_file
    and  (PDVDT_4S.presente!='X' or PDVDT_4S.presente is null)
    ---filtri e legami essitenti
    and  F.filtro_id = 'P'
    AND  D.Tipo_file = decode('P','R','REP','SCN')
    AND  lpad(F.neg_id,4,'0') = lpad(D.neg_id,4,'0')
    AND  decode('P',  'T',data_load_db_test,
        'D',data_load_db_diag,
        'P',data_load_db_proc,
        'R',data_load_db_proc,
        sysdate) is null
    ;
    n_num_righe := SQL%rowcount;
    COMMIT;
    DMS_COM.write_jlt(v_unit, 1,'PROC_TMP_FILE: Blocchi pdv/data inseriti: '|| n_num_righe);

    DMS_COM.write_jlt(v_unit, 1,'---------------------------------------------------------------');
    
  /***************************************************************************************************************
   ***************************************************************************************************************
   *         TERZO PASSO - ricarico e segnalo il ricarico
   ***************************************************************************************************************
   ***************************************************************************************************************/
     
    DMS_COM.write_jlt(v_unit, 1,'------ gestione ricarichi -------------------------------------');

    /******************************************************************************
    per i soli pdv\dt_competenza arrivato da JSON e gia' presenti sulla dw_stato prima
    dell'aggiornamento della stessa: RICARICO
    rimettere a W (in lavorazione) eventuali record gia lavorati (flg_elaborazione=F) 
    di tutte le tabelle interessate
    questo perche' se JSON manda uno scontrino di un pdv\data in ritardo io ricarico tutto
    il pdv\data
    --
    NUOVO CONTROLLO RICHIESTO NELLA RIUNIONE del 201901017: segnalare PDV che per la dt_competenza hanno inviato 2 volte i dati
    (in seguito CONTROLLO PDV\DT reinviati)
    ******************************************************************************/
      select count(*) into v_conta_4s_pdvdt
      from proc_tmp_4s_pdvdt where PRESENTE='Y'
      ;
      
      if v_conta_4s_pdvdt=0 then
         DMS_COM.write_jlt(v_unit, 1,'nessun pdv\dt_competenza da ricaricare'); 
      else
         -------CONTROLLO PDV\DT reinviati : inizializzo PID della mail
         PID := null;
         v_testo:= null;
         -------RICARICO : inzializzo variabili e cerco pdv1dt di interesse (cusrsore RIC_4S)
         n_num_righe := 0;
         v_upd_json  := null;
         
         FOR RIC_4S IN
           (
            select DISTINCT
             to_char(data_file,'yyyy-mm-dd') as DT_SCONTRINO,
             to_number(trim(neg_id))          as COD_PDV
            from proc_tmp_4s_pdvdt
            where PRESENTE='Y'
           )
         LOOP
           -------CONTROLLO PDV\DT reinviati: scrivo testo mail per pdv\dt_competenza
           v_testo:='Warning: pdv '|| to_char(lpad(RIC_4S.cod_pdv,4,'0'))||' ha reinviato scontrini per la data: '||RIC_4S.dt_scontrino ;
           ------->>>>modifica per SVILUPPO
           PID := TESTO_MAIL@dcg_prod(v_testo, PID);
           
           --sincronizziamo le sorgenti impostando in stato Working i record nuovi arrivati finora. Se ne arrivano successivamente non vengono considerat
           DMS_COM.write_jlt(v_unit, 1, 'Inizio ri-sincronizzazione ''Working'' sorgenti per recuperi '||RIC_4S.dt_scontrino||', '||to_char(RIC_4S.cod_pdv));
            update gefa.JSON_SCN_TESTATA@dcg_prod            f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;  n_num_righe := SQL%rowcount;
            update gefa.JSON_SCN_TESTATA_JRET@dcg_prod       f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_RIGHE@dcg_prod              f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_RIGHE_JRET@dcg_prod         f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_FP@dcg_prod                 f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_FP_JRET@dcg_prod            f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_IVA@dcg_prod                f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_TESTATA_CARD@dcg_prod       f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_TESTATA_CARD_JRET@dcg_prod  f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_DETT_CARD@dcg_prod          f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_PROMO@dcg_prod              f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_PROMO_JRET@dcg_prod         f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_ART_VIRTUALI@dcg_prod       f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_ART_VIRTUALI_JRET@dcg_prod  f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_AUTORIZZ_SV@dcg_prod        f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            update gefa.JSON_SCN_EAN_NON_CASSA@dcg_prod      f set f.flg_elaborazione='W' where f.flg_elaborazione='F' and f.dt_scontrino=RIC_4S.dt_scontrino and cod_pdv=RIC_4S.cod_pdv;
            commit;
            DMS_COM.write_jlt(v_unit, 1, 'Effettuata ri-sincronizzazione ''Working'' '||n_num_righe||' scontrini sulle sorgenti');
            
           --pulizia dei doppioni con dt_inserimento precedente
           for cur in (select distinct dt_scontrino ,
                                     cod_insegna ,
                                     cod_pdv ,
                                     cod_cassa ,
                                     cod_scontrino,
                                     dt_inserimento 
                       from gefa.JSON_SCN_TESTATA@dcg_prod s
                      where 1=1
                        and s.cod_pdv = RIC_4S.COD_PDV
                        and s.dt_scontrino = RIC_4S.DT_SCONTRINO
                        and s.flg_elaborazione='W'
                        order by 1,2,3,4,5,6 desc) 
           loop
             
              if(v_dt_scontrino_cur    =cur.dt_scontrino   AND
                   v_cod_insegna_cur   =cur.cod_insegna    AND
                   v_cod_pdv_cur       =cur.cod_pdv        AND
                   v_cod_cassa_cur     =cur.cod_cassa      AND
                   v_cod_scontrino_cur =cur.cod_scontrino) then
                   
                   --update sulla dt inserimento cur.dt_inserimento
                   --sincronizziamo le sorgenti impostando in stato Working i record nuovi arrivati finora. Se ne arrivano successivamente non vengono considerat
                   DMS_COM.write_jlt(v_unit, 1, 'Inizio ri-sincronizzazione ''Fine'' dt_inserimento meno recente sorgenti doppie per recuperi');
                    update gefa.JSON_SCN_TESTATA@dcg_prod            f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;  n_num_righe := SQL%rowcount;
                    update gefa.JSON_SCN_TESTATA_JRET@dcg_prod       f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_RIGHE@dcg_prod              f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;               
                    update gefa.JSON_SCN_RIGHE_JRET@dcg_prod         f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_FP@dcg_prod                 f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento; 
                    update gefa.JSON_SCN_FP_JRET@dcg_prod            f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_IVA@dcg_prod                f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_TESTATA_CARD@dcg_prod       f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_TESTATA_CARD_JRET@dcg_prod  f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_DETT_CARD@dcg_prod          f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_PROMO@dcg_prod              f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_PROMO_JRET@dcg_prod         f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento; 
                    update gefa.JSON_SCN_ART_VIRTUALI@dcg_prod       f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_ART_VIRTUALI_JRET@dcg_prod  f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_AUTORIZZ_SV@dcg_prod        f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    update gefa.JSON_SCN_EAN_NON_CASSA@dcg_prod      f set f.flg_elaborazione='F' where f.flg_elaborazione='W' and dt_scontrino=cur.dt_scontrino and cod_insegna=cur.cod_insegna and cod_pdv=cur.cod_pdv and cod_cassa=cur.cod_cassa and cod_scontrino=cur.cod_scontrino and dt_inserimento=cur.dt_inserimento;
                    commit;
                    
                   DMS_COM.write_jlt(v_unit, 1, 'Effettuata ri-sincronizzazione ''Fine'' dt_inserimento meno recente '||n_num_righe||' scontrini sulle sorgenti');

              end if;
              
                   v_dt_scontrino_cur     :=cur.dt_scontrino;
                   v_cod_insegna_cur      :=cur.cod_insegna;
                   v_cod_pdv_cur          :=cur.cod_pdv;
                   v_cod_cassa_cur        :=cur.cod_cassa;
                   v_cod_scontrino_cur    :=cur.cod_scontrino;
                   v_dt_inserimento_cur   :=cur.dt_inserimento;       
           
           end loop;
            
           DMS_COM.write_jlt(v_unit, 1, 'Effettuata sincronizzazione ''Working'' '||n_num_righe||' scontrini sulle sorgenti');
        -----------unico commit finale
                  COMMIT;
          
         END LOOP;
         
         
         
      end if;
      
      for c in (select * from proc_tmp_4s_pdvdt where presente='X') loop
           v_conta_4s_pdvdt:=v_conta_4s_pdvdt+1;
           v_testo:='Warning: pdv '|| c.neg_id||' ha reinviato scontrini per la data: '||to_char(c.data_file,'yyyy-mm-dd')|| ' fuori dalla data di cutoff ('||v_cutoff||' giorni)' ;
           DMS_COM.write_jlt(v_unit, 1,v_testo);
           PID := TESTO_MAIL@dcg_prod(v_testo, PID);
      end loop;
         -------CONTROLLO PDV\DT reinviati: invio unica mail con elenco pdv|dt_competenza re-inivati e ricaricati
         ------->>>>modifica per SVILUPPO
      if(v_conta_4s_pdvdt>0) then
        SEND_MAIL@dcg_prod('Warning Caricamento scontrini da 4S: Blocchi pdv/data RICARICATI',PID,NUM_GRUPPO_DEST);
      end if;
         
      DMS_COM.write_jlt(v_unit, 1,'Blocchi pdv/data ricaricati da 4s: '|| v_conta_4s_pdvdt);

    DMS_COM.write_jlt(v_unit, 1,'---------------------------------------------------------------');
        

  /***************************************************************************************************************
   ***************************************************************************************************************
   *         QUARTO PASSO - carico in dwh_dati_scn_stor i dati caricati da 4store
   ***************************************************************************************************************
   ***************************************************************************************************************/
 
    DMS_COM.write_jlt(v_unit, 1,'--------- carico dati in dwh_dati_scn_stor  -------------------');

  /***************************************************************************************************************
   *         Popoalmento tabelle TMP per rendere locali i dati gravosi degli scontrini dcg
   ***************************************************************************************************************/
   DMS_COM.write_jlt(v_unit, 1,'--------- ripulisco dati tmp  -------------------');
   
   execute immediate 'truncate table  JSON_SCN_righe_tmp';
   execute immediate 'truncate table  JSON_SCN_promo_tmp';
   execute immediate 'truncate table  JSON_SCN_promo_jret_tmp';
   
   DMS_COM.write_jlt(v_unit, 1,'--------- carico dati JSON_SCN_righe_tmp  -------------------');
   
    insert into JSON_SCN_righe_tmp
     select  * from gefa.JSON_SCN_righe@dcg_prod t
    where dt_scontrino in (
     SELECT  distinct  to_char(PDVDT_4S.DATA_FILE,'yyyy-mm-dd') as DT_SCONTRINO
       FROM proc_tmp_file t,
            PROC_TMP_4S_PDVDT PDVDT_4S
      WHERE  1 = 1
        AND 'SCN'||lpad(PDVDT_4S.NEG_ID,4,'0')||'.'||to_char(PDVDT_4S.DATA_FILE,'RRmmdd')=t.nome_file
        AND  (PDVDT_4S.presente!='X' or PDVDT_4S.presente is null));

   DMS_COM.write_jlt(v_unit, 1,'--------- carico dati JSON_SCN_promo_tmp  -------------------');
    
    insert into JSON_SCN_promo_tmp
     select  * from gefa.JSON_SCN_promo@dcg_prod t
    where dt_scontrino in (
     SELECT  distinct  to_char(PDVDT_4S.DATA_FILE,'yyyy-mm-dd') as DT_SCONTRINO
       FROM proc_tmp_file t,
            PROC_TMP_4S_PDVDT PDVDT_4S
      WHERE  1 = 1
        AND 'SCN'||lpad(PDVDT_4S.NEG_ID,4,'0')||'.'||to_char(PDVDT_4S.DATA_FILE,'RRmmdd')=t.nome_file
        AND  (PDVDT_4S.presente!='X' or PDVDT_4S.presente is null));

   DMS_COM.write_jlt(v_unit, 1,'--------- carico dati JSON_SCN_promo_jret_tmp  -------------------');
    
    insert into JSON_SCN_promo_jret_tmp
     select  * from gefa.JSON_SCN_promo_jret@dcg_prod t
    where dt_scontrino in (
     SELECT  distinct  to_char(PDVDT_4S.DATA_FILE,'yyyy-mm-dd') as DT_SCONTRINO
       FROM proc_tmp_file t,
            PROC_TMP_4S_PDVDT PDVDT_4S
      WHERE  1 = 1
        AND 'SCN'||lpad(PDVDT_4S.NEG_ID,4,'0')||'.'||to_char(PDVDT_4S.DATA_FILE,'RRmmdd')=t.nome_file
        AND  (PDVDT_4S.presente!='X' or PDVDT_4S.presente is null));
     
     commit;    

    /* carico un blocco di scontrini alla volta, per un pdv-giorno completo
       metto in jon la PROC_TMP_PDVDT_4S per segregazione
       ltre a nome file, utile per processo esistente,
       recupero anche pdv e data gia' nel fromato numero e yyyy-mm-dd che c'e' su JSON
       per i filtri */
       
    FOR c_files IN 
    (
     SELECT
      t.nome_file                              as nome_file,
      PDVDT_4S.NEG_ID                          as COD_PDV,
      to_char(PDVDT_4S.DATA_FILE,'yyyy-mm-dd') as DT_SCONTRINO
     FROM proc_tmp_file t,
          PROC_TMP_4S_PDVDT PDVDT_4S
     WHERE  1 = 1
     AND (t.caricato IS NULL OR t.caricato != 'Y')
     AND 'SCN'||lpad(PDVDT_4S.NEG_ID,4,'0')||'.'||to_char(PDVDT_4S.DATA_FILE,'RRmmdd')=t.nome_file
     AND  (PDVDT_4S.presente!='X' or PDVDT_4S.presente is null)
     ORDER  BY t.nome_file
    )
                      
    LOOP
      
    --------tronco la Proc_Dati_Grezzi (come AMZ)
    Execute Immediate '   truncate table PROC_DATI_GREZZI';
    
    DMS_COM.write_jlt(v_unit, 1, '   truncate table PROC_DATI_GREZZI');
    
    /**********************************************************
     * CARICAMENTO EFFETTIVO per pdv\dt_competenza : INIZIO
     * --
     * --NOTA: A regime togliere tutti i DMS_COM.write_jlt di questo llop
     *        sono per pdv\dt_competenza, per i test e' ok , ma a regime sono troppi
     *        identificati da     ----!! solo per test, togliere a regime
     **********************************************************/
      
       DMS_COM.write_jlt(v_unit, 1,'------------------------------------------------------------------------------------------------------------------');
       DMS_COM.write_jlt(v_unit, 1,'###############   CARICAMENTO EFFETTIVO Blocchi pdv/data INIZIO: '||c_files.nome_file ||' - '||c_files.COD_PDV ||' - '||c_files.DT_SCONTRINO ||'###############');
     
      /******************************************************************************    
       *    CREATA TABELLA DI APPOGGIO PROC_TMP_4S_TESTATE 
       *    contiene solo le TESTATE di vendita e reso del PDV\Competenza trattati di volta in volta dal loop
       *    con i campi chiave (tipo_record, pdv,transazione, cassa, competenza)
       *    i campi comuni alle testate di vendita e reso (ora_inizio, num_articoli,tot_netto)
       *    ha in piu' il cod_cliente,cd_fisc_piva,classe che serve solo per testate di vendita 
       *    e che servono per classificare il cliente ed indirizzare gli sconti
       *    (si fa in fase di inserimentonella PROC_DATI_GREZZI)
       *    
       *    La sotto query dt_cli (dati_clienti) lega le righe delle carte al suo file json (dove c'e' la classe cliente) in left join. 
       *    La tabella di configurazione CFG_4S_CLASSECARD riclassifica la tipologia di carta in 01=PRIVILEGE, 02=DIPENDENTI, 00=ALTRO
       *    Le regole sono: se 01 allora valorizzare COD_CLIENTE, se 02 allora valorizzare CD_FISC_PIVA, se 00 valorizzare CLASSE
       *    Inotre: se CDO_CLIENTE e' valoirzzato allora FLG_CLIENTE=1 , 0 altrimenti
       *    Una testata di vendita puo' avere piu' carte associate, quindi anche tutti e 3 i campi valoizzati.
       *    Nel caso ci siano piu' carte anche dello stesso tipo associate, si prende il massimo dei tre campi calcolati (il default dei 3 campi e' quindi 0)
       *    
       *    Valorizzo anche CD_REMOTO (unico altro campo che si ricava dalle carte, cosi' sullle tabelle json delle cartenon entro piu'):
       *    vale 1 se almeno 1 delle carte utilizzate nella testata e' di un pdv diverso da quello dove e' avvenuta la transazione
       *    Sulla DWH_DATI_SCN_STOR ho visto che CD_REMOTO e' sempre voloirzzato, ha defualt 0
       *    
       *    La sotto query dt_cli (dati_clienti) e' legata lal testata in left join perche' una testata puo' non avere nessuna carta. 
       *         
       *    Nella tabella DWH_DATI_SCN_STOR ho visto che:
       *    classe e' sempre voloirzzato, ha defualt 000
       *    COD_CLIENTE puo' essere nullo ed e' di 13 (qeullo che c'e') senza 0 davanti
       *    CD_FISC_PIVA puo' essere nullo, se e' valorizzato e' di 16 con 0 davanti
       *    
       *    Questa tabella serve come filtro sulle chiavi quando si leggono le altre tabelel JSON
       *    per pdv\dt_competenza      
       **********************************************************/
      
         --arricchimento con le battute a reparto per 4STORE
  
         Insert Into Proc_Tmp_Ean
         Select e.ean,e.battuta_reparto,'REP' 
         From   cfg_ean_battute_reparto e
         where e.ean not in (select ean_id from Proc_Tmp_Ean);
  
         V_Nrec:=V_Nrec+Sql%Rowcount;
         Commit;

         If V_Nrec = 0 Then
          Dms_Com.Write_Jlt(V_Unit, 1, 'Nessuna nuova battuta a reparto inserita in PROC_TMP_EAN.');
         Else
          Dms_Com.Write_Jlt(V_Unit, 1, 'Nuove battute a reparto inserite in PROC_TMP_EAN:'||V_Nrec);
         End If ;
         
         update Proc_Tmp_Ean p
         set p.ean_desc='REP'
         where ean_id in (select distinct ean from cfg_ean_battute_reparto e);
         commit;
         
        ------ tronco tutte le tabelle dia ppoggio PROC_TMP_4S_
        
        Execute Immediate 'truncate table PROC_TMP_4S_TESTATE';
        
        DMS_COM.write_jlt(v_unit, 1,'lpad(c_files.COD_PDV,4,''0''):'||lpad(c_files.COD_PDV,4,'0'));
        DMS_COM.write_jlt(v_unit, 1,'c_files.DT_SCONTRINO:'||c_files.DT_SCONTRINO);
        
        /***************************************
         *  inizio gli insert
         *   TESTATE
         *   TABELLA DI APPOGGIO
         ***************************************/
         
        INSERT INTO PROC_TMP_4S_TESTATE
        (
         TIPO_RECORD,
         COD_PDV,
         COD_TRANSAZIONE,
         COD_CASSA,
         DT_COMPETENZA,
         ORA_INIZIO,
         ORA_FINE,
         COD_CLIENTE,
         FLG_CLIENTE,
         CD_REMOTO,
         CD_FISC_PIVA,
         CLASSE,
         NUM_ARTICOLI,
         TOT_NETTO
        )
        (
        SELECT
         CASE WHEN t.COD_TIPO_SCN=(select to_number(valore) from proc_4s_params where alias='scontrino_vendita' and attivo='Y')    THEN '0'
              WHEN t.COD_TIPO_SCN=(select to_number(valore) from proc_4s_params where alias='scontrino_reso_merce' and attivo='Y') THEN '3' 
          END                                                                                                                        as TIPO_RECORD,
         lpad(t.cod_pdv,4,'0')                                                                                                       as COD_PDV,
         lpad(t.COD_SCONTRINO,5,'0')                                                                                                 as COD_TRANSAZIONE,
         lpad(t.COD_CASSA,4,'0')                                                                                                     as COD_CASSA,
         to_date(trim(t.dt_scontrino),'yyyy-mm-dd')                                                                                  as DT_COMPETENZA,
         substr(t.ORA_INIZIO,1,2)||substr(t.ORA_INIZIO,4,2)                                                                          as ORA_INIZIO,
         substr(t.ORA_FINE,1,2)||substr(t.ORA_FINE,4,2)                                                                              as ORA_FINE,
         CASE WHEN substr(trim(dt_cli.COD_CLIENTE),1,16)=lpad('0',16,'0')  THEN null 
              ELSE to_char(to_number(substr(trim(dt_cli.COD_CLIENTE),1,16)))   
          END                                                                                                                        as COD_CLIENTE,
         CASE WHEN NVL( (substr(trim(dt_cli.COD_CLIENTE),1,16)),lpad('0',16,'0') )=lpad('0',16,'0')  THEN '0' 
              ELSE '1'  
          END                                                                                                                        as FLG_CLIENTE,
         NVL(dt_cli.CD_REMOTO,'0')                                                                                                   as CD_REMOTO,
         CASE WHEN substr(trim(dt_cli.CD_FISC_PIVA),1,16)=lpad('0',16,'0') THEN null 
              ELSE LPAD(trim(dt_cli.CD_FISC_PIVA),16,'0') 
          END                                                                                                                        as CD_FISC_PIVA,
         lpad(NVL(dt_cli.CLASSE,'0'),3,'0')                                                                                          as CLASSE,
         abs(t.NUM_TOT_ART)                                                                                                               as NUM_ARTICOLI,
         abs(t.IM_TOT)                                                                                                                    as TOT_NETTO 
        FROM
         ------->>>>modifica per SVILUPPO
         gefa.JSON_SCN_TESTATA@dcg_prod t,
         ---sottoquery dt_cli: inizio
         (
          select 
           lpad(t.cod_pdv,4,'0')                                                                                                             as COD_PDV,
           lpad(t.COD_SCONTRINO,5,'0')                                                                                                       as COD_TRANSAZIONE,
           lpad(t.COD_CASSA,4,'0')                                                                                                           as COD_CASSA,
           to_date(trim(t.dt_scontrino),'yyyy-mm-dd')                                                                                        as DT_COMPETENZA,
           MAX(CASE WHEN TRIM(UPPER(NVL(CLASSECARD.CLASSIFICAZIONE,'00')))=(select to_char(valore) 
                                                                              from proc_4s_params 
                                                                             where alias='privilege_convenzioni' and attivo='Y')
                     THEN t.cod_cliente 
                    ELSE lpad('0',16,'0') 
                END)                                                                                                                         as COD_CLIENTE,
           MAX(CASE WHEN lpad(t.COD_PDV_CLI,4,'0')=lpad(t.COD_PDV,4,'0') THEN '0' 
                    ELSE '1' 
                END)                                                                                                                         as CD_REMOTO,
           MAX(CASE WHEN TRIM(UPPER(NVL(CLASSECARD.CLASSIFICAZIONE,'00')))=(select to_char(valore) 
                                                                              from proc_4s_params 
                                                                             where alias='dipendenti_vip' and attivo='Y') 
                     THEN t.cod_cliente 
                    ELSE lpad('0',16,'0') 
                END)                                                                                                                         as CD_FISC_PIVA,
           MAX(CASE WHEN t.COD_TIPO_CARTA is not null    THEN lpad(t.COD_TIPO_CARTA,3,'0')
                    ELSE lpad('0',3,'0') 
                END)                                                                                                                         as CLASSE 
          from  
                 ------->>>>modifica per SVILUPPO
           gefa.JSON_SCN_TESTATA_CARD@dcg_prod t,
              ------->>>>modifica per SVILUPPO
           gefa.JSON_SCN_TESTATA_CARD_JRET@dcg_prod j,
           ----tabella di configurazione delle tipologie carta
           CFG_4S_CLASSECARD CLASSECARD
          where (1=1)
           ----filtri su testata_card e jref_card
           and trim(upper(t.flg_elaborazione))='W'
           and t.flg_elaborazione=j.flg_elaborazione(+)
           and lpad(t.COD_PDV,4,'0')=lpad(c_files.COD_PDV,4,'0')
           and trim(t.dt_scontrino)=c_files.DT_SCONTRINO
           ---legame card: testata-jret, in left join: potrebbe non esserci la classe
           and t.cod_pdv=j.cod_pdv(+)
           and t.COD_SCONTRINO=j.COD_SCONTRINO(+)
           and t.COD_CASSA=j.COD_CASSA(+)
           and t.dt_scontrino=j.dt_scontrino(+)
           and t.COD_PROGRESSIVO=j.COD_PROGRESSIVO(+)
           ---legame testata_card con tabella di configurazione, in left join per includere anche tipi non configurati (vanno ad altro=00)
           and t.COD_TIPO_CARTA=CLASSECARD.COD_TIPO_CARTA(+)
          group by 
           lpad(t.cod_pdv,4,'0') ,
           lpad(t.COD_SCONTRINO,5,'0') ,
           lpad(t.COD_CASSA,4,'0') ,
           to_date(trim(t.dt_scontrino),'yyyy-mm-dd') 
         ) dt_cli
         ---sottoquery dt_cli: fine
        WHERE (1=1)
         ----filtri su testata 
         and trim(t.COD_TIPO_SCN) in (select to_number(valore) from etl.proc_4s_params where modulo='DWH_PROC_LOAD_SCN_4S' and parametro='COD_TIPO_SCN'  and attivo='Y') 
         and trim(t.COD_STATO_SCN) in (select to_number(valore) from etl.proc_4s_params where modulo='DWH_PROC_LOAD_SCN_4S' and parametro='COD_STATO_SCN' and attivo='Y')
         and trim(upper(t.flg_elaborazione))='W'
         and lpad(t.COD_PDV,4,'0')=lpad(c_files.COD_PDV,4,'0')
         and trim(t.dt_scontrino)=c_files.DT_SCONTRINO
         ----legame testata sottoquery dt_cli in left join
         and lpad(t.cod_pdv,4,'0')=lpad(dt_cli.COD_PDV(+),4,'0')
         and lpad(t.COD_SCONTRINO,5,'0')=dt_cli.COD_TRANSAZIONE(+)
         and lpad(t.COD_CASSA,4,'0')=dt_cli.COD_CASSA(+)
         and to_date(trim(t.dt_scontrino),'yyyy-mm-dd')=dt_cli.DT_COMPETENZA(+)
        )
        ;
        n_num_righe := SQL%rowcount;
        COMMIT;
        ----!! solo per test, togliere a regime
        DMS_COM.write_jlt(v_unit, 1,'######   PROC_TMP_4S_TESTATE, testate vendita e reso: '|| n_num_righe);
        
        /***************************************
         * INSERISCO TESTATE DI VENDITA (tipo_record=0) in PROC_DATI_GREZZI
         *   Aggiungo gli sconti
         *   la sottoquery sc_ar (sconto_articoli) interroga il file delle promo per i record di interesse (pdv e data_competenza passati)
         *        solo per gli sconti articolo che sono sommati per testata, la sottoquery sc_tr (sconto_transazioni) falo stesso solo solo per gli sconti transazione
         *    
         *   per distinguere sconti e sconti fidelity usao il FLG_CLIENTE: se 1-->sconto_fidelity, se 0-->sconto 
         ***************************************/
         
        INSERT INTO PROC_DATI_GREZZI
        (
         TIPO_RECORD,
         COD_PDV,
         COD_TRANSAZIONE,
         COD_CASSA,
         DT_COMPETENZA,
         ORA_INIZIO,
         ORA_FINE,
         COD_CLIENTE,   
         FLG_CLIENTE,
         CD_REMOTO,
         CD_FISC_PIVA,  
         CLASSE,
         NUM_ARTICOLI,
         TOT_NETTO,
         SCONTO_TRANSAZ,
         SCONTO_ARTICOLI,
         SCONTO_TRANSAZ_FIDELITY,
         SCONTO_ARTICOLI_FIDELITY,
         BOLLINI_SCONTRINO,       ----SEMPRE 0 su indicazione Molteni
         DT_CARICAMENTO,          ----SEMPRE sysdate
         FLAG_ELAB                ----SEMPRE N   
        )
        (
        SELECT
         TESTATE_4S.TIPO_RECORD                                                                              as TIPO_RECORD,
         TESTATE_4S.COD_PDV                                                                                  as COD_PDV,
         TESTATE_4S.COD_TRANSAZIONE                                                                          as COD_TRANSAZIONE,
         TESTATE_4S.COD_CASSA                                                                                as COD_CASSA,
         TESTATE_4S.DT_COMPETENZA                                                                            as DT_COMPETENZA,
         TESTATE_4S.ORA_INIZIO                                                                               as ORA_INIZIO,
         TESTATE_4S.ORA_FINE                                                                                 as ORA_FINE,
         TESTATE_4S.COD_CLIENTE                                                                              as COD_CLIENTE,
         TESTATE_4S.FLG_CLIENTE                                                                              as FLG_CLIENTE,
         TESTATE_4S.CD_REMOTO                                                                                as CD_REMOTO,
         TESTATE_4S.CD_FISC_PIVA                                                                             as CD_FISC_PIVA,
         TESTATE_4S.CLASSE                                                                                   as CLASSE,
         abs(TESTATE_4S.NUM_ARTICOLI)                                                                        as NUM_ARTICOLI,
         abs(TESTATE_4S.TOT_NETTO)                                                                           as TOT_NETTO,
         CASE WHEN NVL(TRIM(TESTATE_4S.FLG_CLIENTE),'0')='0' THEN NVL(abs(sc_tr.SCONTO),0) ELSE 0 END        as SCONTO_TRANSAZ,
         CASE WHEN NVL(TRIM(TESTATE_4S.FLG_CLIENTE),'0')='0' THEN NVL(abs(sc_ar.SCONTO),0) ELSE 0 END        as SCONTO_ARTICOLI,
         CASE WHEN NVL(TRIM(TESTATE_4S.FLG_CLIENTE),'0')='1' THEN NVL(abs(sc_tr.SCONTO),0) ELSE 0 END        as SCONTO_TRANSAZ_FIDELITY,
         CASE WHEN NVL(TRIM(TESTATE_4S.FLG_CLIENTE),'0')='1' THEN NVL(abs(sc_ar.SCONTO),0) ELSE 0 END        as SCONTO_ARTICOLI_FIDELITY,
         0                                                                                                   as BOLLINI_SCONTRINO,
         sysdate                                                                                             as DT_CARICAMENTO,
         'N'                                                                                                 as FLAG_ELAB
        FROM   
         PROC_TMP_4S_TESTATE TESTATE_4S
         ,
         ---sottoquery sc_ar: inizio
         (
            select lpad(t.cod_pdv,4,'0')                      as COD_PDV,
                   lpad(t.COD_SCONTRINO,5,'0')                as COD_TRANSAZIONE,
                   lpad(t.COD_CASSA,4,'0')                    as COD_CASSA,
                   to_date(trim(t.dt_scontrino),'yyyy-mm-dd') as DT_COMPETENZA,
                   SUM(NVL(IM_PROMO,0))                       as SCONTO
            from JSON_SCN_promo_tmp t,
                 ETL.DCG_CF_SCONTI_4S@dcg_prod sc
            where (1=1)
             and t.cod_azione=sc.cod_az_comm
             and sc.flg_tipo_sconto='A'
             and trim(upper(t.flg_elaborazione))='W'
             and lpad(t.COD_PDV,4,'0')=lpad(c_files.COD_PDV,4,'0')
             and trim(t.dt_scontrino)=c_files.DT_SCONTRINO
            group by lpad(t.cod_pdv,4,'0'),
                     lpad(t.COD_SCONTRINO,5,'0'),
                     lpad(t.COD_CASSA,4,'0'),
                     to_date(trim(t.dt_scontrino),'yyyy-mm-dd')
         ) sc_ar
         ---sottoquery sc_ar: fine
         ,
         ---sottoquery sc_tr: inizio
         (
           select lpad(t.cod_pdv,4,'0')                      as COD_PDV,
                   lpad(t.COD_SCONTRINO,5,'0')                as COD_TRANSAZIONE,
                   lpad(t.COD_CASSA,4,'0')                    as COD_CASSA,
                   to_date(trim(t.dt_scontrino),'yyyy-mm-dd') as DT_COMPETENZA,
                   SUM(NVL(IM_PROMO,0))                       as SCONTO
            from JSON_SCN_promo_tmp t,
                 ETL.DCG_CF_SCONTI_4S@dcg_prod sc
            where (1=1)
             and t.cod_azione=sc.cod_az_comm
             and sc.flg_tipo_sconto='T'
             and trim(upper(t.flg_elaborazione))='W'
             and lpad(t.COD_PDV,4,'0')=lpad(c_files.COD_PDV,4,'0')
             and trim(t.dt_scontrino)=c_files.DT_SCONTRINO
            group by lpad(t.cod_pdv,4,'0'),
                     lpad(t.COD_SCONTRINO,5,'0'),
                     lpad(t.COD_CASSA,4,'0'),
                     to_date(trim(t.dt_scontrino),'yyyy-mm-dd')
         ) sc_tr
         ---sottoquery sc_tr: fine
        WHERE (1=1)
         ---solo testate di vendita TIPO_RECORD=0 con COD_CLIENTE (id_card) valorizzato
         and TESTATE_4S.TIPO_RECORD='0'
         ---legame testate-sconti in left join puo' esserci testata senza sconti: 
         ---legame con sconti articolo
         and lpad(TESTATE_4S.COD_PDV,4,'0')=lpad(sc_ar.COD_PDV(+),4,'0')
         and TESTATE_4S.COD_TRANSAZIONE=sc_ar.COD_TRANSAZIONE(+)
         and TESTATE_4S.COD_CASSA=sc_ar.COD_CASSA(+)
         and TESTATE_4S.DT_COMPETENZA=sc_ar.DT_COMPETENZA(+)
         ---legame con sconti transazione 
         and lpad(TESTATE_4S.COD_PDV,4,'0')=lpad(sc_tr.COD_PDV(+),4,'0')
         and TESTATE_4S.COD_TRANSAZIONE=sc_tr.COD_TRANSAZIONE(+)
         and TESTATE_4S.COD_CASSA=sc_tr.COD_CASSA(+)
         and TESTATE_4S.DT_COMPETENZA=sc_tr.DT_COMPETENZA(+)
        )
        ; 
        n_num_righe := SQL%rowcount;
        COMMIT;
        ----!! solo per test, togliere a regime
        DMS_COM.write_jlt(v_unit, 1,'######   PROC_DATI_GREZZI, testate vendita: '|| n_num_righe);

        /***************************************
         *  testate di RESO
         *   calcolo campi specifici per il reso:
         *   sotto query j_reso
         *    dal file jret delle testate prendo i jret degli scontrini di reso:
         *    almeno uno tra NUM_DOC_RESO,MATRICOLA_CASSA_RESO,DT_RESO,ORA_DOC_RESO valorizzati
         *    e sopratutto in inner join con le testate di reso: TESTATE_4S.TIPO_RECORD='3'
         *    per i jret di reso prendo:
         *    NUM_DOC_RESO: numero del documento commerciale originale 
         *    ---> serve per rientrare sul file jret e trovare lo scontrino da cui si e' generato il reso
         *    DT_RESO: data del documento commerciale originale 
         *    --> RESO_DT_TRANSAZIONE (Fromato date, come la dt_competenza)
         *    ORA_DOC_RESO: ora del documento commerciale originale
         *    --> RESO_ORA_TRANSAZIONE nel formato substr(lpad(replace(j.ORA_DOC_RESO,':',''),6,'0'),1,4)
         *    ZNUMBER: ZNumber restituito dalla funzione GetStatus da utilizzare per i documenti di annullo e reso
         *    --> RESO_NUM_AZZERAMENTO 
         *    sotto query j_vendita
         *    dal file jret delle testate prendo i jret degli scontrini di vendita:
         *    NUM_DOC_RESO,MATRICOLA_CASSA_RESO,DT_RESO,ORA_DOC_RESO TUTTI NON valorizzati
         *    per i jret di vendita prendo:
         *    NUM_DOC: numero del documento di vendita, serve per legarsi al docuemto di reso
         *    ---> in left join con NUM_DOC_RESO recupoerato da sotto query j_reso 
         *      (left perche' il NUM_DOC della vendita potrebbe essere stato cancellato se anteriore a 30 giorni il reso)
         *      COD_PDV e COD_CASSA (trasformati gia' nel formato giusto, con 0 a sinistra)
         *      --> RESO_NUM_CASSA =PDV+CASSA
         *      COD_SCONTRINO: (gia' nel formato giusto con 0 a sinistra, come COD_TRANSAZIONE)
         *      --> RESO_NUM_TRANSAZIONE 
         ***************************************/
              
        INSERT INTO PROC_DATI_GREZZI
        (
         TIPO_RECORD,
         COD_PDV,
         COD_TRANSAZIONE,
         COD_CASSA,
         DT_COMPETENZA,
         ORA_INIZIO,
         ORA_FINE,
         FLG_CLIENTE,   ----in questo caso sempre 0
         NUM_ARTICOLI,
         TOT_NETTO,
         RESO_NUM_CASSA,
         RESO_NUM_TRANSAZIONE,
         RESO_NUM_AZZERAMENTO,
         RESO_DT_TRANSAZIONE,
         RESO_ORA_TRANSAZIONE,
         DT_CARICAMENTO,----SEMPRE sysdate
         FLAG_ELAB      ----SEMPRE N   
        )
        (
        SELECT
         TESTATE_4S.TIPO_RECORD                                                        as TIPO_RECORD,
         TESTATE_4S.COD_PDV                                                            as COD_PDV,
         TESTATE_4S.COD_TRANSAZIONE                                                    as COD_TRANSAZIONE,
         TESTATE_4S.COD_CASSA                                                          as COD_CASSA,
         TESTATE_4S.DT_COMPETENZA                                                      as DT_COMPETENZA,
         TESTATE_4S.ORA_INIZIO                                                         as ORA_INIZIO,
         TESTATE_4S.ORA_FINE                                                           as ORA_FINE,
         TESTATE_4S.FLG_CLIENTE                                                        as FLG_CLIENTE,
         abs(TESTATE_4S.NUM_ARTICOLI)                                                  as NUM_ARTICOLI,
         abs(TESTATE_4S.TOT_NETTO)                                                     as TOT_NETTO,
         j_vendita.RESO_NUM_CASSA                                                      as RESO_NUM_CASSA,
         j_vendita.RESO_NUM_TRANSAZIONE                                                as RESO_NUM_TRANSAZIONE,
         j_reso.RESO_NUM_AZZERAMENTO                                                   as RESO_NUM_AZZERAMENTO,
         j_reso.RESO_DT_TRANSAZIONE                                                    as RESO_DT_TRANSAZIONE,
         j_reso.RESO_ORA_TRANSAZIONE                                                   as RESO_ORA_TRANSAZIONE,
         sysdate                                                                       as DT_CARICAMENTO,
         'N'                                                                           as FLAG_ELAB
        FROM   
         PROC_TMP_4S_TESTATE TESTATE_4S
         left join (select ---campi chiave per join con PRPC_TMP_4S_TESTATE, gia' nel fromato giusto per la join
                           lpad(j.cod_pdv,4,'0')                                   as COD_PDV,
                           lpad(j.COD_SCONTRINO,5,'0')                             as COD_TRANSAZIONE,
                           lpad(j.COD_CASSA,4,'0')                                 as COD_CASSA,
                           to_date(trim(j.dt_scontrino),'yyyy-mm-dd')              as DT_COMPETENZA,
                           ----campi del jret di reso che servono per recupoerare lo scontrino da cui si e' generato il reso
                           j.NUM_DOC_RESO                                          as NUM_DOC_RESO,
                           ----campi dello scontrino da cui si e' generato il reso recuperabili direttamente dal jret dello scontrino di reso
                           trim(ZNUMBER)                                           as RESO_NUM_AZZERAMENTO,
                           ( lpad(j.cod_pdv,4,'0') )||( lpad(substr(j.MATRICOLA_CASSA_RESO,-3),4,'0') )  as RESO_NUM_CASSA,
                           --to_date(trim(j.DT_RESO),'yyyy-mm-dd')                   as RESO_DT_TRANSAZIONE,
                           case when length(trim(j.DT_RESO))=10 then to_date(trim(j.DT_RESO),'yyyy-mm-dd') else null end as RESO_DT_TRANSAZIONE,
                           substr(lpad(replace(j.ORA_DOC_RESO,':',''),6,'0'),1,4)  as RESO_ORA_TRANSAZIONE
                    from gefa.JSON_SCN_TESTATA_JRET@dcg_prod j
                    where (1=1)
                      and (
                          trim(NUM_DOC_RESO)         is not null or
                          trim(MATRICOLA_CASSA_RESO) is not null or
                          trim(DT_RESO)              is not null or
                          trim(ORA_DOC_RESO)         is not null
                          )
                  ) j_reso on (lpad(j_reso.COD_PDV,4,'0')    = lpad(TESTATE_4S.COD_PDV,4,'0')
                                 and j_reso.COD_TRANSAZIONE  = TESTATE_4S.COD_TRANSAZIONE
                                 and j_reso.COD_CASSA        = TESTATE_4S.COD_CASSA
                                 and j_reso.DT_COMPETENZA    = TESTATE_4S.DT_COMPETENZA)
          left join (select  ----campo del jret di vendita che identifica lo scontrino da cui si e' generato il reso
                             j.NUM_DOC                                               as NUM_DOC,
                             ----campi dello scontrino da cui si e' generato il reso recuperati reintrando per NUM_DOC_RESO sul NUM_DOC dello scontrino di vendita
                             ( lpad(j.cod_pdv,4,'0') )||( lpad(j.COD_CASSA,4,'0') )  as RESO_NUM_CASSA,
                             lpad(j.COD_SCONTRINO,5,'0')                             as RESO_NUM_TRANSAZIONE
                       from gefa.JSON_SCN_TESTATA_JRET@dcg_prod j
                      where (1=1)
                        --and lpad(COD_PDV,4,'0')=lpad(c_files.COD_PDV,4,'0')
                        --and trim(dt_scontrino)=c_files.DT_SCONTRINO
                        and trim(NUM_DOC_RESO)         is null
                        and trim(MATRICOLA_CASSA_RESO) is null
                        and trim(DT_RESO)              is null
                        --and trim(ORA_DOC_RESO) is null --si vede che non e' null sul documento di vendita collegato
                    ) j_vendita on (j_reso.NUM_DOC_RESO       = j_vendita.NUM_DOC
                                    and j_reso.RESO_NUM_CASSA = j_vendita.RESO_NUM_CASSA)
        WHERE (1=1)
        ---solo testate di reso TIPO_RECORD=3
         and TESTATE_4S.TIPO_RECORD='3'
        /*---legame jret di reso con testate di RESO
         and lpad(j_reso.COD_PDV,4,'0')=lpad(TESTATE_4S.COD_PDV,4,'0')
         and j_reso.COD_TRANSAZIONE=TESTATE_4S.COD_TRANSAZIONE
         and j_reso.COD_CASSA=TESTATE_4S.COD_CASSA
         and j_reso.DT_COMPETENZA=TESTATE_4S.DT_COMPETENZA
        ---legame jret di reso con jret di vendita (left join)
         and j_reso.NUM_DOC_RESO=j_vendita.NUM_DOC(+)
         and j_reso.RESO_NUM_CASSA=j_vendita.RESO_NUM_CASSA*/
        )
        ;
        n_num_righe := SQL%rowcount;
        COMMIT;
        ----!! solo per test, togliere a regime    
        DMS_COM.write_jlt(v_unit, 1,'######   PROC_DATI_GREZZI, testate reso con campi di scontrino originale: '|| n_num_righe);

        /***************************************
         * RIGHE
         *   Aggiungo sconti, vedi descrizione per testate vendita
         *   ora pero' nelle sotto query non sommo per testata, ma scendo a livello di riga
         *   nella sottoquery sc_tr la seconda parte della union serve a prendere la parte non ventilata:
         *   il residuo non ventilato non ha id_riga, allora prendo tutte le righe di una transazione, valoirzzo a 0 il residuo se ? nullo, 
         *   per transazione prendo il minomi riga_rif e sommo il residuo, cioe' attribbiisco il residuo alla prima riga di vendita della transazione 
         ***************************************/
        
        /*************************************************************************/
           if(v_res='OK') then
              v_res:=RicalcolaImportiCesti(lpad(c_files.COD_PDV,4,'0'),c_files.DT_SCONTRINO);
              DMS_COM.write_jlt(v_unit, 1, 'RicalcolaImportiCesti: '||v_res);
           else
             DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
             DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
             DMS_LAT.elab_fail(SQLERRM);
           end if;
        /*************************************************************************/
   
        INSERT INTO PROC_DATI_GREZZI
        (
         TIPO_RECORD,
         COD_PDV,
         COD_TRANSAZIONE,
         COD_CASSA,
         DT_COMPETENZA,
         ORA_INIZIO,        ----solo tp_rec=5, reso
         ORA_FINE,          ----solo tp_rec=2, vendita
         FLG_CLIENTE,
         COD_EAN,
         COD_ART_REP,
         TIPO_RIGA,
         TIPO_SCONTO,       ----solo tp_rec=2, vendita
         FLAG_ART_REP,
         DEPT,
         QUANTITA,
         PESO_PER_1000,
         VALORE_NETTO_RIGA,
         SCONTO_RIGA,         ----solo tp_rec=2, vendita
         SCONTO_FIDELITY,     ----solo tp_rec=2, vendita
         SCONTO_TRX_VENTILATO,----solo tp_rec=2, vendita
         BOLLINI_RIGA,      ----SEMPRE null su indicazione Molteni
         DT_CARICAMENTO,    ----SEMPRE sysdate
         FLAG_ELAB          ----SEMPRE N  
        )
        (
        SELECT /*+ NO_USE_NL(t sc_ar sc_tr testate_4s Cdi)*/
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN '2' ELSE '5' END                                               as TIPO_RECORD,
         TESTATE_4S.COD_PDV                                                                                       as COD_PDV,
         TESTATE_4S.COD_TRANSAZIONE                                                                               as COD_TRANSAZIONE,
         TESTATE_4S.COD_CASSA                                                                                     as COD_CASSA,
         TESTATE_4S.DT_COMPETENZA                                                                                 as DT_COMPETENZA,
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN null ELSE substr(lpad(replace(t.ORA,':',''),6,'0'),1,4) END    as ORA_INIZIO,   ----solo tp_rec=5, reso
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN substr(lpad(replace(t.ORA,':',''),6,'0'),1,4) ELSE null END    as ORA_FINE,     ----solo tp_rec=2, vendita
         TESTATE_4S.FLG_CLIENTE                                                                                   as FLG_CLIENTE,
         substr(trim(t.COD_EAN),1,13)                                                                             as COD_EAN,
         case when (Nvl(ean.art_id,'x')='x' OR ean.ean_desc='REP') then 'RR'||lpad(trim(t.COD_REPARTO),4,'0') 
              else Lpad(ean.art_id,6,'0') 
         end                                                                                                      as COD_ART_REP,
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN '1' ELSE '2' END                                               as TIPO_RIGA,
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN '000' ELSE null END                                            as TIPO_SCONTO,  ----solo tp_rec=2, vendita
         ----!!! da determinare, per ora null!!!!
         Decode(Nvl(ean.ean_desc,''),'REP','R','A')                                                               as FLAG_ART_REP,
         lpad(t.COD_REPARTO,4,'0')                                                                                as DEPT,
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' 
           THEN (t.QTA)               --vendita
           else -(t.QTA)            --reso
         END                                                                                                      as QUANTITA,
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' 
           THEN (t.QTA * t.PESO_KG * 1000)      --vendita (nelle righe di annullo si ha qta negativa per perso positivo
           else -(t.PESO_KG*1000)   --reso
         END                                                                                                      as PESO_PER_1000,
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' 
           THEN (t.IM_NETTO_PROMO)    --vendita
           else -(t.IM_NETTO_PROMO) --reso
         END                                                                                                      as VALORE_NETTO_RIGA,
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN ----solo tp_rec=2, vendita
           CASE WHEN NVL(TRIM(TESTATE_4S.FLG_CLIENTE),'0')='0' THEN NVL(abs(sc_ar.importo_sconto),0)*SIGN(t.QTA)  ELSE 0 END
         ELSE null END                                                                                            as SCONTO_RIGA,----solo tp_rec=2, vendita
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN ----solo tp_rec=2, vendita
           CASE WHEN NVL(TRIM(TESTATE_4S.FLG_CLIENTE),'0')='1' THEN NVL(abs(sc_ar.importo_sconto),0)*SIGN(t.QTA)  ELSE 0 END
         ELSE null END                                                                                            as SCONTO_FIDELITY,----solo tp_rec=2, vendita
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN NVL(abs(sc_tr.importo_sconto),0)*SIGN(t.QTA) ELSE null END     as SCONTO_TRX_VENTILATO,----solo tp_rec=2, vendita
         0                                                                                                        as BOLLINI_RIGA,
         sysdate                                                                                                  as DT_CARICAMENTO,
         'N'                                                                                                      as FLAG_ELAB
        FROM   
              ------->>>>modifica per SVILUPPO
         JSON_SCN_righe_tmp t,
         Proc_Tmp_Ean                 Ean,
         Com_Cdi_Art_Dit@Dwh_To_Dcc    Cdi,
         PROC_TMP_4S_TESTATE          TESTATE_4S
         ,
         ---sottoquery sc_ar: inizio
         (
                select   n.dt_scontrino,
                         n.cod_insegna,
                         n.cod_pdv,
                         n.COD_CASSA,
                         n.COD_SCONTRINO,
                         n.flg_tipo_sconto,
                         n.cod_riga_rif, 
                         r.cod_articolo,
                         sum(importo_sconto) as importo_sconto from 
                (select p.dt_scontrino,
                        p.cod_insegna,
                        p.cod_pdv,
                        p.cod_cassa,
                        p.cod_scontrino,
                        case when p.cod_riga_rif=0 then j.cod_riga_rif else p.cod_riga_rif end cod_riga_rif,
                        sc.flg_tipo_sconto,
                        nvl(-j.im_promo_ventilato,p.im_promo) importo_sconto
                 from JSON_SCN_promo_tmp  p 
                 join ETL.DCG_CF_SCONTI_4S@dcg_prod sc on (p.cod_azione=sc.cod_az_comm and sc.flg_tipo_sconto is not null)
            left join JSON_SCN_promo_jret_tmp j on (p.dt_scontrino=j.dt_scontrino
                                                    and p.cod_insegna=j.cod_insegna
                                                    and p.cod_pdv=j.cod_pdv
                                                    and p.cod_cassa=j.cod_cassa
                                                    and p.cod_scontrino=j.cod_scontrino
                                                    and p.cod_riga=j.cod_riga)
                 UNION ALL
                 select  re.dt_scontrino,
                         re.cod_insegna,
                         re.cod_pdv,
                         re.COD_CASSA,
                         re.COD_SCONTRINO,
                         MIN(re.COD_RIGA_RIF)         as COD_RIGA_RIF,
                         sc.flg_tipo_sconto,
                         SUM( NVL(re.IM_RIMANENZA,0)) as importo_sconto
                 from JSON_SCN_promo_jret_tmp re,
                      JSON_SCN_promo_tmp p
            left join ETL.DCG_CF_SCONTI_4S@dcg_prod sc on (p.cod_azione=sc.cod_az_comm and sc.flg_tipo_sconto is not null)
                 where p.dt_scontrino=re.dt_scontrino
                   and p.cod_insegna=re.cod_insegna
                   and p.cod_pdv=re.cod_pdv
                   and p.cod_cassa=re.cod_cassa
                   and p.cod_scontrino=re.cod_scontrino
                   and p.cod_riga=re.cod_riga
                 group by re.COD_SCONTRINO,
                          re.cod_insegna,
                          re.cod_pdv,
                          re.COD_CASSA,
                          re.dt_scontrino,
                          sc.flg_tipo_sconto) n,
                  JSON_SCN_righe_tmp    r                        
                where 1=1
                and n.dt_scontrino=r.dt_scontrino
                and n.cod_insegna=r.cod_insegna
                and n.cod_pdv=r.cod_pdv
                and n.cod_cassa=r.cod_cassa
                and n.cod_scontrino=r.cod_scontrino
                and n.cod_riga_rif=r.cod_riga(+)
                and flg_tipo_sconto='A'
                group by n.dt_scontrino,
                         n.cod_insegna,
                         n.cod_pdv,
                         n.COD_CASSA,
                         n.COD_SCONTRINO,
                         n.flg_tipo_sconto,
                         n.cod_riga_rif, 
                         r.cod_articolo   
         ) sc_ar
         ---sottoquery sc_ar: fine
         ,
         ---sottoquery sc_tr: inizio
         (
                select   n.dt_scontrino,
                         n.cod_insegna,
                         n.cod_pdv,
                         n.COD_CASSA,
                         n.COD_SCONTRINO,
                         n.flg_tipo_sconto,
                         n.cod_riga_rif, 
                         r.cod_articolo,
                         sum(importo_sconto) as importo_sconto from 
                (select p.dt_scontrino,
                        p.cod_insegna,
                        p.cod_pdv,
                        p.cod_cassa,
                        p.cod_scontrino,
                        case when p.cod_riga_rif=0 then j.cod_riga_rif else p.cod_riga_rif end cod_riga_rif,
                        sc.flg_tipo_sconto,
                        nvl(-j.im_promo_ventilato,p.im_promo) importo_sconto
                 from JSON_SCN_promo_tmp  p 
                 join ETL.DCG_CF_SCONTI_4S@dcg_prod sc on (p.cod_azione=sc.cod_az_comm and sc.flg_tipo_sconto is not null)
            left join JSON_SCN_promo_jret_tmp j on (p.dt_scontrino=j.dt_scontrino
                                                    and p.cod_insegna=j.cod_insegna
                                                    and p.cod_pdv=j.cod_pdv
                                                    and p.cod_cassa=j.cod_cassa
                                                    and p.cod_scontrino=j.cod_scontrino
                                                    and p.cod_riga=j.cod_riga)
                 UNION ALL
                 select  re.dt_scontrino,
                         re.cod_insegna,
                         re.cod_pdv,
                         re.COD_CASSA,
                         re.COD_SCONTRINO,
                         MIN(re.COD_RIGA_RIF)         as COD_RIGA_RIF,
                         sc.flg_tipo_sconto,
                         SUM( NVL(re.IM_RIMANENZA,0)) as importo_sconto
                 from JSON_SCN_promo_jret_tmp re,
                      JSON_SCN_promo_tmp p
            left join ETL.DCG_CF_SCONTI_4S@dcg_prod sc on (p.cod_azione=sc.cod_az_comm and sc.flg_tipo_sconto is not null)
                 where p.dt_scontrino=re.dt_scontrino
                   and p.cod_insegna=re.cod_insegna
                   and p.cod_pdv=re.cod_pdv
                   and p.cod_cassa=re.cod_cassa
                   and p.cod_scontrino=re.cod_scontrino
                   and p.cod_riga=re.cod_riga
                 group by re.COD_SCONTRINO,
                          re.cod_insegna,
                          re.cod_pdv,
                          re.COD_CASSA,
                          re.dt_scontrino,
                          sc.flg_tipo_sconto) n,
                  JSON_SCN_righe_tmp    r                        
                where 1=1
                and n.dt_scontrino=r.dt_scontrino
                and n.cod_insegna=r.cod_insegna
                and n.cod_pdv=r.cod_pdv
                and n.cod_cassa=r.cod_cassa
                and n.cod_scontrino=r.cod_scontrino
                and n.cod_riga_rif=r.cod_riga(+)
                and flg_tipo_sconto='T'
                group by n.dt_scontrino,
                         n.cod_insegna,
                         n.cod_pdv,
                         n.COD_CASSA,
                         n.COD_SCONTRINO,
                         n.flg_tipo_sconto,
                         n.cod_riga_rif, 
                         r.cod_articolo   
         ) sc_tr
         ---sottoquery sc_tr: fine
        WHERE (1=1)
         and cdi.last_flg (+) = 1 -- prendo sempre l'ultimo valido
         and trim(t.COD_EAN) = to_char(ean.ean_id (+))
         and ean.art_id = cdi.art_cod (+)
        ----filtri su righe 
         and t.cod_tipo_riga in (select to_number(valore) from etl.proc_4s_params where modulo='DWH_PROC_LOAD_SCN_4S' and parametro='COD_TIPO_RIGA' and attivo='Y') 
         and t.cod_funzione  in (select to_number(valore) from etl.proc_4s_params where modulo='DWH_PROC_LOAD_SCN_4S' and parametro='COD_FUNZIONE'  and attivo='Y')
         and trim(upper(t.flg_elaborazione))='W'
         and lpad(t.COD_PDV,4,'0')=lpad(c_files.COD_PDV,4,'0')
         and trim(t.dt_scontrino)=c_files.DT_SCONTRINO
         and t.cod_sorgente_lettura not in (select to_number(valore) from etl.proc_4s_params where modulo='DWH_PROC_LOAD_SCN_4S' and parametro='COD_COMPONENTI_CESTI'  and attivo='Y') --escludiamo i componenti dei cesti
        ---legame righe-testate
         and lpad(t.cod_pdv,4,'0')=TESTATE_4S.COD_PDV
         and lpad(t.COD_SCONTRINO,5,'0')=TESTATE_4S.COD_TRANSAZIONE
         and lpad(t.COD_CASSA,4,'0')=TESTATE_4S.COD_CASSA
         and to_date(trim(t.dt_scontrino),'yyyy-mm-dd')=TESTATE_4S.DT_COMPETENZA 
        ---legame righe-sconti in left join puo' esserci testata senza sconti: 
         ---legame con sconti articolo
         and lpad(t.cod_pdv,4,'0')=lpad(sc_ar.COD_PDV(+),4,'0')
         and t.COD_SCONTRINO=sc_ar.COD_SCONTRINO(+)
         and t.COD_CASSA=sc_ar.COD_CASSA(+)
         and t.dt_scontrino=sc_ar.dt_scontrino(+)
         and t.COD_RIGA=sc_ar.cod_riga_rif(+)
         ---legame con sconti transazione 
         and lpad(t.cod_pdv,4,'0')=lpad(sc_tr.COD_PDV(+),4,'0')
         and t.COD_SCONTRINO=sc_tr.COD_SCONTRINO(+)
         and t.COD_CASSA=sc_tr.COD_CASSA(+)
         and t.dt_scontrino=sc_tr.dt_scontrino(+)
         and t.COD_RIGA=sc_tr.cod_riga_rif(+)   
        )
        ;
        n_num_righe := SQL%rowcount;
        COMMIT;
        ----!! solo per test, togliere a regime
        DMS_COM.write_jlt(v_unit, 1,'######   PROC_DATI_GREZZI, righe vendita e reso: '|| n_num_righe);

        /***************************************
         * FP
         ***************************************/
         
        INSERT INTO PROC_DATI_GREZZI
        (
         TIPO_RECORD,
         COD_PDV,
         COD_TRANSAZIONE,
         COD_CASSA,
         DT_COMPETENZA,
         ORA_INIZIO,
         FLG_CLIENTE,
         TIPO_PAGAM,
         VALORE_NETTO,
         DT_CARICAMENTO,    ----SEMPRE sysdate
         FLAG_ELAB          ----SEMPRE N  
        )
        (    
        SELECT
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN '1' ELSE '4' END as TIPO_RECORD,
         TESTATE_4S.COD_PDV                                         as COD_PDV,
         TESTATE_4S.COD_TRANSAZIONE                                 as COD_TRANSAZIONE,
         TESTATE_4S.COD_CASSA                                       as COD_CASSA,
         TESTATE_4S.DT_COMPETENZA                                   as DT_COMPETENZA,
         min(substr(lpad(replace(t.ORA,':',''),6,'0'),1,4))         as ORA_INIZIO,
         TESTATE_4S.FLG_CLIENTE                                     as FLG_CLIENTE,
         lpad(trim(FP_4S.DWH_PROC_TIPO_PAGAM),2,'0')                as TIPO_PAGAM,
         sum(CASE WHEN TESTATE_4S.TIPO_RECORD='0' 
               THEN ((t.qta * t.IM_STANDARD))  --vendita
               else (abs(t.qta * t.IM_STANDARD)) --reso
         END)                                                       as VALORE_NETTO,
         sysdate                                                    as DT_CARICAMENTO,
         'N'                                                        as FLAG_ELAB
        FROM   
              ------->>>>modifica per SVILUPPO
         gefa.JSON_SCN_FP@dcg_prod t,
         PROC_TMP_4S_TESTATE TESTATE_4S,
         ------tabella trascodifica forme_pagamento
              ------->>>>modifica per SVILUPPO
         etl.DCG_CF_FP_4S@dcg_prod FP_4S
        WHERE (1=1)
        ----filtri su righe 
         and trim(upper(t.flg_elaborazione))='W'
         and lpad(t.COD_PDV,4,'0')=lpad(c_files.COD_PDV,4,'0')
         and trim(t.dt_scontrino)=c_files.DT_SCONTRINO
        ---legame con testate
         and lpad(t.cod_pdv,4,'0')=lpad(TESTATE_4S.COD_PDV,4,'0')
         and lpad(t.COD_SCONTRINO,5,'0')=TESTATE_4S.COD_TRANSAZIONE
         and lpad(t.COD_CASSA,4,'0')=TESTATE_4S.COD_CASSA
         and to_date(trim(t.dt_scontrino),'yyyy-mm-dd')=TESTATE_4S.DT_COMPETENZA
        ---legame con trascodifica FP
         and t.COD_FP=trim(FP_4S.cod_mod_pag)
        GROUP BY
         CASE WHEN TESTATE_4S.TIPO_RECORD='0' THEN '1' ELSE '4' END,
         TESTATE_4S.COD_PDV,
         TESTATE_4S.COD_TRANSAZIONE,
         TESTATE_4S.COD_CASSA,
         TESTATE_4S.DT_COMPETENZA,
         TESTATE_4S.FLG_CLIENTE,
         lpad(trim(FP_4S.DWH_PROC_TIPO_PAGAM),2,'0')
        )
        ;
        n_num_righe := SQL%rowcount;
        COMMIT;
        
        ----!! solo per test, togliere a regime    
        DMS_COM.write_jlt(v_unit, 1,'######   PROC_DATI_GREZZI, forme pagamento vendita e reso: '|| n_num_righe);     
        --DMS_COM.write_jlt(v_unit, 1,'###############   CARICAMENTO EFFETTIVO Blocchi pdv/data FINE: '||c_files.nome_file ||' - '||c_files.COD_PDV ||' - '||c_files.DT_SCONTRINO ||'###############');
        DMS_COM.write_jlt(v_unit, 1,'------------------------------------------------------------------------------------------------------------------');
            
        
        /***************************************
         * innesto su procedura esistente   
         ***************************************/
          
          -- considero il pdv corrente appena caricato e che ha il flag_elab null o diverso da 'Y' e da 'E', errato
      begin
        select distinct s.cod_pdv
          into v_cod_pdv
          from PROC_DATI_GREZZI s
         where s.flag_elab IS NULL OR s.flag_elab not in('Y','E');
         
         --bisogna uniformare ora_inizio perche' 4store invia dati i dati di pagamento e righe con ora inizio che puo' essere diversa dalla testata
         for cur in (select t.cod_pdv,t.cod_transazione,t.cod_cassa,t.dt_competenza,t.ora_inizio 
                       from PROC_DATI_GREZZI t, 
                            dcg_an_pdv@dcg_prod a
                      where tipo_record in ('0','3')
                        and a.flg_4s='Y' 
                        and t.dt_competenza>=a.dt_avvio_4s and t.dt_competenza>=trunc(to_date('08/09/2021','dd/mm/yyyy'))
                        and t.cod_pdv='0'||substr(a.cd_entity,3)
                      order by dt_competenza desc,cod_pdv,cod_transazione,cod_cassa) loop
                      
                      
                      update /*+ index(  proc_dati_grezzi proc_dwh_dati_idx1) */ PROC_DATI_GREZZI
                         set ora_inizio=cur.ora_inizio
                       where tipo_record not in ('0','3')
                         and cod_pdv=cur.cod_pdv
                         and cod_transazione=cur.cod_transazione
                         and cod_cassa=cur.cod_cassa
                         and dt_competenza=cur.dt_competenza;
           
         end loop;

         DMS_COM.write_jlt(v_unit, 1, 'uniformato ora_inizio su PROC_DATI_GREZZI');
          commit;
          
        if(substr(c_files.nome_file,4,4) <> v_cod_pdv )then

          --creazione di una mail di riepilogo (DA ATTIVARE IN PRODUZIONE)

          PID := null;
          v_testo:='Warning: pdv '|| v_cod_pdv||' nei dati degli scontrini diverso dal pdv riportato come nome del file dati:'||c_files.nome_file ;
          ------->>>>modifica per SVILUPPO
          PID := TESTO_MAIL@dcg_prod(v_testo, PID);
          DMS_COM.write_jlt(v_unit, 1, v_testo );

          ------->>>>modifica per SVILUPPO
          SEND_MAIL@dcg_prod('Warning su caricamento scontrino SCN',PID,NUM_GRUPPO_DEST);

          --- segno come errato il cod_pdv nella PROC_DATI_GREZZI per non leggerlo alla iterazione successiva
          update PROC_DATI_GREZZI s
             set s.flag_elab = 'E'
           where s.cod_pdv = v_cod_pdv;


        else

          DMS_COM.write_jlt(v_unit, 1, '###   caricati gli scn sulla proc_dati_grezzi per il file '|| c_files.nome_file );

            delete dwh_dati_scn_stor d
             where (lpad(d.cod_pdv,4,'0') , d.dt_competenza) IN
                   (SELECT distinct lpad(s.cod_pdv,4,'0') , s.dt_competenza
                      FROM PROC_DATI_GREZZI s
                      where s.flag_elab IS NULL OR s.flag_elab not in('Y','E') );

          DMS_COM.write_jlt(v_unit, 1, '###   cancellati '||to_char(SQL%rowcount)||' records');
          commit;

          insert  into dwh_dati_scn_stor d
          select
           tipo_record                   ,
           case when substr(cod_pdv,1,1)!='0' then cod_pdv else substr(cod_pdv,2) end,
           cod_transazione               ,
           cod_cassa                     ,
           dt_competenza                 ,
           ora_inizio                    ,
           ora_fine                      ,
           cod_cliente                   ,
           flg_cliente                   ,
           cd_remoto                     ,
           cd_fisc_piva                  ,
           classe                        ,
           num_articoli                  ,
           tot_netto                     ,
           sconto_transaz                ,
           sconto_articoli               ,
           sconto_transaz_fidelity       ,
           sconto_articoli_fidelity      ,
           bollini_scontrino             ,
           tipo_pagam                    ,
           valore_netto                  ,
           to_number(cod_ean)            ,
           cod_art_rep                   ,
           tipo_riga                     ,
           tipo_sconto                   ,
           flag_art_rep                  ,
           dept                          ,
           quantita                      ,
           peso_per_1000                 ,
           valore_netto_riga             ,
           sconto_riga                   ,
           sconto_fidelity               ,
           sconto_trx_ventilato          ,
           bollini_riga                  ,
           to_char(dt_competenza, 'rrmmdd')||cod_pdv||cod_cassa||cod_transazione ,
           v_sysdate                     ,
           null                          ,
           null                          ,
           null                          ,
           null                          ,
           reso_num_cassa                ,
           reso_num_transazione          ,
           reso_num_azzeramento          ,
           reso_dt_transazione           ,
           reso_ora_transazione          ,
           'N'
            from PROC_DATI_GREZZI s
           where(s.flag_elab IS NULL OR s.flag_elab not in('Y','E'));

          DMS_COM.write_jlt(v_unit, 1, '###   inseriti in dwh_dati_scn_stor: '||to_char(SQL%rowcount)||' records');

          commit;

          update proc_tmp_file
             set caricato = 'Y'
           where nome_file = c_files.nome_file;

          -- aggiorno il flag per indicare che il pdv corrente e' stato caricato
          update PROC_DATI_GREZZI s
             set s.flag_elab = 'Y'
           where s.cod_pdv = v_cod_pdv;

           commit;

        end if;

      commit;
      
      exception
        WHEN no_data_found THEN
          DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
          DMS_COM.write_jlt(v_unit, 1, 'Warning!! pdv non trovato  ' || v_cod_pdv );
      end;

    END LOOP;

    DMS_COM.write_jlt(v_unit, 1,'---------------------------------------------------------------');
    
    /*************************************************************************/
       if(v_res='OK') then
          v_res:=MailRiepilogo4S(sysdate);
          DMS_COM.write_jlt(v_unit, 1, 'MailRiepilogo4S: '||v_res);
       else
         DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
         DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
         DMS_LAT.elab_fail(SQLERRM);
       end if;
    /*************************************************************************/
        

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DWH_PROC_LOAD_SCN_4S;
  
/******************************************************************************
     NAME:       DWH_PROC_SCN_PROCESSING
     PURPOSE:    Procedura che rielabora i dati degli scontrini caricati
                 al passo precedente

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        22/11/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE DWH_PROC_SCN_PROCESSING
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'DWH_PROC_SCN_PROCESSING';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_count               NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_ora                 varchar2(4);
    v_scn_key             VARCHAR2(50);


    v_art_id              DWH_DATI_SCN_STOR.COD_ART_REP%TYPE;
    v_res                 VARCHAR2(2):='OK';
    PID                   NUMBER;
 
    NUM_GRUPPO_DEST VARCHAR2(10) := '14';
    v_testo         VARCHAR2(4000);   
    v_count_fp      INTEGER:=0; 

  BEGIN
   --impostiamo un riferimento temporale univoco su cui basare tutte le sincronizzazioni.
   --select sysdate+1 into v_sysdate from dual;
   select sysdate into v_sysdate from dual;
   DMS_COM.write_jlt(v_unit, 1, 'Impostazione riferimento temporale: '||to_char(v_sysdate,'dd/mm/yyyy hh24:mi:ss'));
    -- INIZIO TRACE
    -- dbms_session.set_sql_trace(TRUE);

    dbms_output.enable(40000000);

    execute immediate 'truncate table PROC_ARTICOLIUNES';
    execute immediate 'truncate table PROC_EAN';
    execute immediate 'truncate table PROC_PROMO';

    DMS_COM.write_jlt(v_unit, 1, 'truncate table PROC_ARTICOLIUNES');
    DMS_COM.write_jlt(v_unit, 1, 'truncate table PROC_EAN');
    DMS_COM.write_jlt(v_unit, 1, 'truncate table PROC_PROMO');

    --parificazione preliminare flg_cliente tra testata e altre righe (serve per velocizzare altre operazioni)

    SELECT d.*
      BULK COLLECT INTO v_Scn
      from DWH_DATI_SCN_STOR d,proc_tmp_file t
                where 1=1
                and d.tipo_record=0
                and lpad(d.cod_pdv,4,'0') =substr(t.nome_file, 4, 4)
                and d.dt_competenza=to_date(substr(t.nome_file, 9), 'rrmmdd')
                and t.caricato='Y';

    DMS_COM.write_jlt(v_unit, 1, 'update DWH_DATI_SCN_STOR per scn_key');

      for idx_scn in 1 .. v_Scn.COUNT
        loop

           update DWH_DATI_SCN_STOR p
           set p.flg_cliente = v_Scn(idx_scn).flg_cliente,
               p.cod_cliente = v_Scn(idx_scn).cod_cliente
           where p.scn_key   = v_Scn(idx_scn).scn_key
             and p.tipo_record in (1,2);

        end loop;

    COMMIT;

    --caricamento EAN
    DMS_COM.write_jlt(v_unit, 1, 'caricamento  PROC_EAN');

/*************************************************************************/
    SELECT *
      BULK COLLECT INTO v_Ean
      FROM (SELECT  art_id,
                    ean_id
            FROM    anag.Ean
            WHERE art_id is not null
              AND ean_id is not null
            ORDER BY ean_id) tabella;


     FORALL idx_ean IN 1 .. v_Ean.COUNT
      INSERT INTO proc_ean
      VALUES
      v_Ean(idx_ean);

    COMMIT;
     --pulizia della collection non piu' utilizzata
     DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Ean');

     v_Ean.DELETE;

     --caricamento PROMO
     DMS_COM.write_jlt(v_unit, 1, 'caricamento  PROC_PROMO');

/*************************************************************************/
    SELECT *
      BULK COLLECT INTO v_Promo
      FROM (select pa.art_id,
                   p.promo_id,
                   neg_id,
                   to_date(substr(t.nome_file,9),'rrmmdd')
              from anag.promozione_articolo pa,
                   anag.promozione_negozio pn,
                   anag.promozione p,
                   proc_tmp_file t
             where pa.promo_id=p.promo_id
             and pn.promo_id=p.promo_id
             and to_date(substr(t.nome_file,9),'rrmmdd') between p.data_inizio and p.data_fine
             and lpad(neg_id,4,'0') = substr(t.nome_file,4,4)
             and t.caricato='Y'
             order by pa.art_id) tabella;


     FORALL idx_promo IN 1 .. v_Promo.COUNT
      INSERT INTO proc_promo
      VALUES
      v_Promo(idx_promo);

    COMMIT;
    --pulizia della collection non piu' utilizzata
    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Promo');

    v_Promo.DELETE;

    --caricamento SCN
/*************************************************************************/
    --pulizia della collection usata in prececenza
    DMS_COM.write_jlt(v_unit, 1, 'pulizia collection v_Scn');

    v_Scn.DELETE;

    DMS_COM.write_jlt(v_unit, 1, 'caricamento SCN');

    
                
    SELECT d.*
      BULK COLLECT INTO v_Scn
      FROM DWH_DATI_SCN_STOR d, proc_tmp_file t
      where d.dt_competenza=to_date(substr(t.nome_file,9),'rrmmdd')
        and lpad(d.cod_pdv,4,'0')=substr(t.nome_file,4,4)
        and t.caricato='Y'
        and rownum<2; --limite imposto x la versione alternativa. da togliere quando si torna sui bulk loads
      
      --versione alternativa senza bulk loads
      /*for cur in (SELECT \*+ NO_USE_NL(d t) *\ d.*
              FROM DWH_DATI_SCN_STOR d, proc_tmp_file t
              where d.dt_competenza=to_date(substr(t.nome_file,9),'rrmmdd')
                and lpad(d.cod_pdv,4,'0')=substr(t.nome_file,4,4)
                and t.caricato='Y') loop
      --for idx_scn in 1 .. v_Scn.COUNT 
         --loop

         if((cur.tipo_record=2 or cur.tipo_record=5)
            and
            cur.flag_art_rep='A')
         then
           v_cicli:=v_cicli+1;
           begin
               select p.art_id
               into v_art_id
               from proc_ean p
               where p.ean=cur.cod_ean;

               v_found:=v_found+SQL%rowcount;




            UPDATE \*+ index(d IDX7_PROC_STOR)*\ DWH_DATI_SCN_STOR d
            SET d.cod_art_rep=v_art_id
            where d.scn_key=cur.scn_key
              and d.tipo_record=cur.tipo_record
              and d.cod_ean=cur.cod_ean
              and d.flag_art_rep=cur.flag_art_rep;

           v_count:=v_count+SQL%rowcount;

           exception when NO_DATA_FOUND then

           v_exceptions:=v_exceptions+1;

             UPDATE \*+ index(d IDX7_PROC_STOR)*\ DWH_DATI_SCN_STOR d
            SET d.cod_art_rep=null
            where d.scn_key=cur.scn_key
              and d.tipo_record=cur.tipo_record
              and d.cod_ean=cur.cod_ean
              and d.flag_art_rep=cur.flag_art_rep;

            v_count_exc:=v_count_exc+SQL%rowcount;
           end;

         end if;
         
         
         if(MOD(v_cicli,1000)=0) 
         then 
           commit;
         end if;
         
         if(MOD(v_cicli,100000)=0) 
         then 
           DMS_COM.write_jlt(v_unit, 1, '#cicli: '||to_char(v_cicli));
           
         end if;
         

      end loop;

      dbms_output.put_line('#cicli: '||to_char(v_cicli));
      dbms_output.put_line('#trovati: '||to_char(v_found));
      dbms_output.put_line('#modificati: '||to_char(v_count));
      dbms_output.put_line('#eccezioni trovate: '||to_char(v_exceptions));
      dbms_output.put_line('#eccezioni modificate: '||to_char(v_count_exc));


      DMS_COM.write_jlt(v_unit, 1, '#cicli: '||to_char(v_cicli));
      DMS_COM.write_jlt(v_unit, 1, '#trovati: '||to_char(v_found));
      DMS_COM.write_jlt(v_unit, 1, '#modificati: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#eccezioni trovate: '||to_char(v_exceptions));
      DMS_COM.write_jlt(v_unit, 1, '#eccezioni modificate: '||to_char(v_count_exc));*/
      
      UPDATE /*+ index(d IDX1_PROC_STOR)*/ DWH_DATI_SCN_STOR d
            SET d.cod_art_rep=null
            where 1=1
            and ((d.tipo_record=2 or d.tipo_record=5)
                  and
                 d.flag_art_rep='A')
            and (d.dt_competenza,lpad(d.cod_pdv,4,'0')) in (select to_date(substr(t.nome_file,9),'rrmmdd'),substr(t.nome_file,4,4)
                                                               from proc_tmp_file t
                                                               where t.caricato='Y');
                                                               
      commit;
                
      UPDATE /*+ index(d IDX1_PROC_STOR)*/ DWH_DATI_SCN_STOR d
            SET d.cod_art_rep=(select case when p.art_id is not null then p.art_id else null end
               --into v_art_id
               from proc_ean p
               where p.ean=d.cod_ean)
            where 1=1
            and ((d.tipo_record=2 or d.tipo_record=5)
                  and
                 d.flag_art_rep='A')
            and (d.dt_competenza,lpad(d.cod_pdv,4,'0')) in (select to_date(substr(t.nome_file,9),'rrmmdd'),substr(t.nome_file,4,4)
                                                               from proc_tmp_file t
                                                               where t.caricato='Y');

    COMMIT;
    
       /*******************************************************/
    /***     allargamento scn_key con orario scontrino   ***/
    /*******************************************************/
    DMS_COM.write_jlt(v_unit, 1, 'inizio allargamento scn_key con orario scontrino');
    for c in (select d.rowid,d.* from dwh_dati_scn_stor d, proc_tmp_file t
               where nvl(d.flg_introduzione_ora,'N')='N'
                 and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                 and lpad(d.cod_pdv,4,'0') = substr(t.nome_file, 4, 4)
                 and t.caricato = 'Y'
              order by decode(d.ora_inizio,null,d.ora_fine,d.ora_inizio),d.tipo_record
      ) loop
      
        if(c.tipo_record='0') then v_ora:=c.ora_inizio; end if;
        
        v_scn_key:=c.scn_key || v_ora;
        
        update dwh_dati_scn_stor set scn_key=v_scn_key, flg_introduzione_ora='Y'
        where rowid=c.rowid;
                              
    
    end loop;
    
    commit;
    DMS_COM.write_jlt(v_unit, 1, 'fine allargamento scn_key con orario scontrino');

    ---compattamento dello scontrino --forme di pagamento
    DMS_COM.write_jlt(v_unit, 1, 'compattamento dello scontrino --forme di pagamento');

/*************************************************************************/
  SELECT * BULK COLLECT
    INTO v_Chiave_Scn
    from (select distinct dt_competenza,
                          cod_pdv,
                          cod_cassa,
                          cod_transazione,
                          scn_key
            from (select count(*) conteggio,
                         dt_competenza,
                         cod_pdv,
                         cod_cassa,
                         cod_transazione,
                         tipo_record,
                         tipo_pagam,
                         d.scn_key
                    from dwh_dati_scn_stor d, proc_tmp_file t
                   where (tipo_record = 1 or tipo_record = 4)--FP
                     and d.dt_competenza = to_date(substr(t.nome_file, 9), 'rrmmdd')
                     and lpad(d.cod_pdv,4,'0') = substr(t.nome_file, 4, 4)
                     and t.caricato = 'Y'
                   group by dt_competenza,
                            cod_pdv,
                            cod_cassa,
                            cod_transazione,
                            tipo_record,
                            tipo_pagam,
                            d.scn_key
                  having count(*) > 1
                   order by dt_competenza,
                            cod_pdv,
                            cod_cassa,
                            cod_transazione,
                            tipo_record));


      v_cicli := 0;
      v_count:=0;
      v_found:=0;
      v_count_exc:=0;

    --DMS_COM.write_jlt(v_unit, 1, 'segno da cancellare i record che devo ricoprire');

     for idx_key in 1 .. v_Chiave_Scn.COUNT
       loop
         v_cicli:=v_cicli+1;
       --segno da cancellare i record che devo ricoprire
       update DWH_DATI_SCN_STOR d
       set d.dt_caricamento=null
        where d.scn_key=v_Chiave_Scn(idx_key).scn_key
          and (tipo_record = 1 or tipo_record = 4);

       v_found:=v_found+SQL%rowcount;

    --DMS_COM.write_jlt(v_unit, 1, 'inserisco i nuovi record raggruppati');

       --inserisco i nuovi record raggruppati
       insert  into DWH_DATI_SCN_STOR
          select tipo_record,cod_pdv,cod_transazione,cod_cassa,
                 dt_competenza,ora_inizio,ora_fine,cod_cliente,
                 flg_cliente,cd_remoto,cd_fisc_piva,classe,
                 num_articoli,tot_netto,sconto_transaz,sconto_articoli,
                 sconto_transaz_fidelity,sconto_articoli_fidelity,
                 bollini_scontrino,tipo_pagam,
                 sum(valore_netto),
                 cod_ean, cod_art_rep,tipo_riga,tipo_sconto,flag_art_rep,dept,
                 quantita,peso_per_1000,valore_netto_riga,sconto_riga,
                 sconto_fidelity,sconto_trx_ventilato,bollini_riga,
                 scn_key,sysdate,promo_id,num_sco_cli,tipo_promo_id, scontrino_prog,
                 reso_num_cassa,reso_num_transazione,reso_num_azzeramento,
                 reso_dt_transazione,reso_ora_transazione,flg_introduzione_ora
            from dwh_dati_scn_stor
           where (tipo_record = 1 or tipo_record = 4)
             and scn_key = v_Chiave_Scn(idx_key).scn_key
           group by tipo_record,cod_pdv,cod_transazione,cod_cassa,
                 dt_competenza,ora_inizio,ora_fine,cod_cliente,
                 flg_cliente,cd_remoto,cd_fisc_piva,classe,
                 num_articoli,tot_netto,sconto_transaz,sconto_articoli,
                 sconto_transaz_fidelity,sconto_articoli_fidelity,
                 bollini_scontrino,tipo_pagam,
                 cod_ean, cod_art_rep,tipo_riga,tipo_sconto,flag_art_rep,dept,
                 quantita,peso_per_1000,valore_netto_riga,sconto_riga,
                 sconto_fidelity,sconto_trx_ventilato,bollini_riga,
                 scn_key,promo_id,num_sco_cli,tipo_promo_id, scontrino_prog,
                 reso_num_cassa,reso_num_transazione,reso_num_azzeramento,
                 reso_dt_transazione,reso_ora_transazione,flg_introduzione_ora;


         v_count:=v_count+SQL%rowcount;

         --DMS_COM.write_jlt(v_unit, 1, 'cancello i vecchi record');
         --cancello i vecchi record

         delete DWH_DATI_SCN_STOR d
         where d.scn_key=v_Chiave_Scn(idx_key).scn_key
          and (tipo_record = 1 or tipo_record = 4)
          and dt_caricamento is null;

         v_count_exc:=v_count_exc+SQL%rowcount;

      end loop;

      dbms_output.put_line(' ');
      dbms_output.put_line('#cicli di raggruppamento fp: '||to_char(v_cicli));
      dbms_output.put_line('#marcati null: '||to_char(v_found));
      dbms_output.put_line('#reinseriti: '||to_char(v_count));
      dbms_output.put_line('#cancellazioni: '||to_char(v_count_exc));

      DMS_COM.write_jlt(v_unit, 1, '#cicli di raggruppamento fp: '||to_char(v_cicli));
      DMS_COM.write_jlt(v_unit, 1, '#marcati null: '||to_char(v_found));
      DMS_COM.write_jlt(v_unit, 1, '#reinseriti: '||to_char(v_count));
      DMS_COM.write_jlt(v_unit, 1, '#cancellazioni: '||to_char(v_count_exc));


   COMMIT;

/*************************************************************************/
   if(v_res='OK') then
      v_res:=VerificaDept();
      dbms_output.put_line('VerificaDept: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'VerificaDept: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=CompattaRigheScn();
      dbms_output.put_line('CompattaRigheScn: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'CompattaRigheScn: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=TempRicalcolaBollini();
      dbms_output.put_line('TempRicalcolaBollini: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'TempRicalcolaBollini: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=AttribuisciPromo();
      dbms_output.put_line('AttribuisciPromo: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'AttribuisciPromo: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=AggiornaCodFisc();
      dbms_output.put_line('AggiornaCodFisc: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'AggiornaCodFisc: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=AggiornaArticoliGiorno();
      dbms_output.put_line('AggiornaArticoliGiorno: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'AggiornaArticoliGiorno: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=ScaricaTestata();
      dbms_output.put_line('ScaricaTestata: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'ScaricaTestata: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=ScaricaFormePagamento();
      dbms_output.put_line('ScaricaFormePagamento: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'ScaricaFormePagamento: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=ScaricaRigheEAnomalie();
      dbms_output.put_line('ScaricaRigheEAnomalie: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'ScaricaRigheEAnomalie: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=ScaricaArtSettCorrente();
      dbms_output.put_line('ScaricaArtSettCorrente: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'ScaricaArtSettCorrente: '||v_res);
   end if;
/*************************************************************************/
   if(v_res='OK') then
      v_res:=ConsolidaDwStatoScn();
      dbms_output.put_line('ConsolidaDwStatoScn: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'ConsolidaDwStatoScn: '||v_res);
   end if;
/*************************************************************************/

   COMMIT;

    --Creazione Di Una Mail Di Riepilogo (Da Attivare In Produzione)
    Pid := Null;
    V_Testo:='Esito : '|| V_res||' '||' '  ;
    Pid := Testo_Mail@Dcg_Prod(V_Testo, Pid);
    --aggiunta di un warning per le FP 4STORE non gestite
    select count(*)
      into v_count_fp
      from GEFA.JSON_SCN_FP@dcg_prod t
      left join etl.dcg_cf_fp_4s@dcg_prod c on (c.cod_mod_pag=t.cod_fp)
     where c.cod_mod_pag is null;
     
    if(v_count_fp>0)
    then 
       Pid := Testo_Mail@Dcg_Prod('', Pid);
       V_Testo:='ATTENZIONE FORMA DI PAGAMENTO NON CENSITA NEI FLUSSI 4STORE:'  ;
       Pid := Testo_Mail@Dcg_Prod(V_Testo, Pid);
    
      for c_fp in (select distinct c.cod_mod_pag,c.descrizione,t.cod_fp,t.desc_fp 
                     from GEFA.JSON_SCN_FP@dcg_prod t
                     left join etl.dcg_cf_fp_4s@dcg_prod c on (c.cod_mod_pag=t.cod_fp)
                    where c.cod_mod_pag is null) 
      loop        
        V_Testo:='FP : '|| c_fp.cod_fp||' - '||c_fp.desc_fp  ;
        Pid := Testo_Mail@Dcg_Prod(V_Testo, Pid);
      end loop;
    end if;
    
    Send_Mail@Dcg_Prod('Terminazione flusso scontrini SCN',Pid,Num_Gruppo_Dest);


   -- FINE TRACE
   -- dbms_session.set_sql_trace(FALSE);


  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DWH_PROC_SCN_PROCESSING;

/******************************************************************************
     NAME:       DWH_PROC_REP_PROCESSING
     PURPOSE:    Procedura che ricostruisce i dati rep a partire dai dati SCN
                 presenti nella tabella DWH_DATI_SCN_STOR

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        15/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE DWH_PROC_REP_PROCESSING
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'DWH_PROC_REP_PROCESSING';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_count               NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;


    v_p11                 VARCHAR2(8);
    v_art_id              DWH_DATI_SCN_STOR.COD_ART_REP%TYPE;
    v_res                 VARCHAR2(2):='OK';
    v_prosegui            CHAR(1):='N';
    v_giorno_attuale      VARCHAR2(3);
    v_day_start           DATE;

    --DA ATTIVARE IN PRODUZIONE
    NUM_GRUPPO_DEST         VARCHAR2(10) := '100';
    PID                     NUMBER;
    v_testo                 VARCHAR2(2000);


  BEGIN
    dbms_output.enable(40000000);

    select to_char(sysdate, 'DY')
    into v_giorno_attuale
    from dual;

    dbms_output.put_line('Il codice viene eseguito solo il '|| p9 || ' o per un lunedi'' eplicito');
    DMS_COM.write_jlt(v_unit, 1, 'Il codice viene eseguito solo il '|| p9 || ' o per un lunedi'' eplicito');

    if (v_giorno_attuale=p9 OR v_giorno_attuale=p10) then
      v_prosegui:='Y';
    end if;


    if (p11 is not null AND p11!='ND') then
      if(to_char(to_date(p11,'yyyymmdd'),'DY')='LUN' or
         to_char(to_date(p11,'yyyymmdd'),'DY')='MON') then

        v_p11:=p11;
        v_prosegui:='Y';
      else
        dbms_output.put_line('Il giorno ('||p11||') indicato nel parametro non e'' un lunedi''');
        v_p11:=p11;
      end if;
    else
        v_p11:=to_char((sysdate-7),'yyyymmdd');
    end if;

    dbms_output.put_line('v_p11:'||v_p11);
    DMS_COM.write_jlt(v_unit, 1, 'v_p11:'||v_p11);

    select dt_in_settimana
      into v_day_start
      from DIM_CALENDARIO_SETT t
     where to_number(v_p11) between dt_in_settimana_k and dt_fi_settimana_k;

if(v_prosegui='Y') then

  /*************************************************************************/
   if(v_res='OK') then
    DMS_COM.write_jlt(v_unit, 1, 'LoadRepClass v_day_start: '||v_day_start);
      v_res:=LoadRepClass(v_day_start);
      dbms_output.put_line('LoadRepClass: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'LoadRepClass: '||v_res);
   end if;
  /*************************************************************************/
   if(v_res='OK') then
      v_res:=RicavaDatiRep(v_day_start);
      dbms_output.put_line('RicavaDatiRep: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'RicavaDatiRep: '||v_res);
   end if;
  /*************************************************************************/
   if(v_res='OK') then
      v_res:=GestisciRep(v_day_start);
      dbms_output.put_line('GestisciRep: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'GestisciRep: '||v_res);
   end if;
  /*************************************************************************/
   if(v_res='OK') then
      v_res:=ScaricaRep();
      dbms_output.put_line('ScaricaRep: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'ScaricaRep: '||v_res);
   end if;
  /*************************************************************************/
   if(v_res='OK') then
      v_res:=ConsolidaDwStatoRep();
      dbms_output.put_line('ConsolidaDwStatoRep: '||v_res);
      DMS_COM.write_jlt(v_unit, 1, 'ConsolidaDwStatoRep: '||v_res);
   end if;
/*************************************************************************/

--creazione di una mail di riepilogo (DA ATTIVARE IN PRODUZIONE)

  PID := null;
  v_testo:=' Elaborati i riepologhi REP per la settimana del '||to_char(v_day_start,'dd/mm/yyyy');
  PID := TESTO_MAIL@DCG_PROD(v_testo, PID);

  SEND_MAIL@DCG_PROD('Ricalcolo dati REP',PID,NUM_GRUPPO_DEST);

else
  --se e' indicata la data di inizio partiamo da quel lunedi', senno' dal lunedi' della settimana precedente a quella corrente
  dbms_output.put_line('p9:'||p9||'; p10:'||p10||'; p11:'||p11||';');
  DMS_COM.write_jlt(v_unit, 1, 'p9:'||p9||'; p10:'||p10||'; p11:'||p11||';');
  DMS_COM.write_jlt(v_unit, 1, 'Job terminato');
end if;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DWH_PROC_REP_PROCESSING;

/******************************************************************************
     NAME:       DWH_PROC_ANOMALIE
     PURPOSE:    Procedura che risolve le righe scontrino anomale

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        23/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE DWH_PROC_ANOMALIE
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'DWH_PROC_ANOMALIE';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_count               NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_promo               VARCHAR2(11);
    v_test                NUMBER;
    v_test_proc           VARCHAR2(16);
  BEGIN
    dbms_output.enable(40000000);
    DMS_COM.write_jlt(v_unit, 1, 'inizio modulo');

    for cur_diag in (SELECT distinct r.scontrino_prog as scontrino_prog,
                             r.art_id as old_art_id,
                             'AA' || lpad(r.rep_pos_id, 4, '0') as art_id,
                             r.tipo_riga_id as tipo_riga_id,
                             r.tipo_art_id as tipo_art_id,
                             r.neg_id as neg_id,
                             to_char(r.data, 'DDMMYYYY') as data,
                             r.cli_id as cli_id,
                             r.fs_flag as fs_flag,
                             r.rep_pos_id as rep_pos_id,
                             nvl(r.promo_id, ' ') as promo_id,
                             r.qta as qta,
                             r.peso as peso ,
                             r.imp_netto as imp_netto,
                             r.fs_sconto as fs_sconto,
                             r.nfs_sconto as nfs_sconto,
                             r.bollini as bollini,
                             r.fs_sconto_trx as fs_sconto_trx,
                             r.rowid
                        FROM diag.riga_anomalia r, anag.filtro_negozio fn
                       WHERE lpad(r.neg_id,4,'0') = lpad(fn.neg_id,4,'0')
                         AND r.flg_verificato='N'
                         AND (fn.filtro_id = 'A' or fn.filtro_id = 'D'))
    loop
        v_cicli:=v_cicli+1;
        begin

          SELECT p.promo_id
          INTO v_promo
          FROM promozione_articolo pa, promozione_negozio pn, promozione p
         WHERE pa.promo_id = p.promo_id
           AND pn.promo_id = p.promo_id
           AND to_date(cur_diag.data, 'DDMMYYYY') between p.data_inizio AND p.data_fine
           AND lpad(pn.neg_id,4,'0') = lpad(cur_diag.neg_id,4,'0')
           AND pa.art_id = cur_diag.art_id;

        exception when no_data_found then
          v_promo:=' ';
        end;

       begin

         select scontrino_prog
         into v_test
         from diag.riga
         where scontrino_prog=cur_diag.scontrino_prog
          and art_id=cur_diag.art_id
          and tipo_riga_id=cur_diag.tipo_riga_id;

         update diag.riga u
            set u.qta = u.qta + cur_diag.qta,
                u.peso = u.peso + cur_diag.peso,
                u.imp_netto = u.imp_netto + cur_diag.imp_netto,
                u.fs_sconto = u.fs_sconto + cur_diag.fs_sconto,
                u.nfs_sconto = u.nfs_sconto + cur_diag.nfs_sconto,
                u.bollini = u.bollini + cur_diag.bollini,
                u.fs_sconto_trx = u.fs_sconto_trx + cur_diag.fs_sconto_trx
          where u.scontrino_prog=cur_diag.scontrino_prog
          and u.art_id=cur_diag.art_id
          and u.tipo_riga_id=cur_diag.tipo_riga_id;

         v_count:=v_count+SQL%rowcount;

       exception when no_data_found then

         insert  into diag.riga i
         select cur_diag.scontrino_prog,
                cur_diag.art_id,
                cur_diag.tipo_riga_id,
                cur_diag.tipo_art_id,
                cur_diag.neg_id,
                to_date(cur_diag.data,'ddmmyyyy'),
                cur_diag.cli_id,
                cur_diag.fs_flag,
                cur_diag.rep_pos_id,
                v_promo,
                cur_diag.qta,
                cur_diag.peso,
                cur_diag.imp_netto,
                cur_diag.fs_sconto,
                cur_diag.nfs_sconto,
                cur_diag.bollini,
                cur_diag.fs_sconto_trx,
                null as ora
         from dual;
         v_count_exc:=v_count_exc+SQL%rowcount;
       end;


        update  diag.riga_anomalia a
        set   a.flg_verificato='Y'
        where a.scontrino_prog=cur_diag.scontrino_prog
          and a.art_id=cur_diag.old_art_id
          and a.tipo_riga_id=cur_diag.tipo_riga_id;

    end loop;

    dbms_output.put_line('#cicli diag: '||to_char(v_cicli));
    dbms_output.put_line('#insert diag: '||to_char(v_count_exc));
    dbms_output.put_line('#update diag: '||to_char(v_count));

    DMS_COM.write_jlt(v_unit, 1, '#cicli diag: '||to_char(v_cicli));
    DMS_COM.write_jlt(v_unit, 1, '#insert diag: '||to_char(v_count_exc));
    DMS_COM.write_jlt(v_unit, 1, '#update diag: '||to_char(v_count));

    COMMIT;

    v_cicli:=0;
    v_count_exc:=0;
    v_count:=0;

    for cur_proc in (SELECT distinct r.scontrino_prog as scontrino_prog,
                                     'AA' || lpad(r.rep_pos_id, 4, '0') as art_id,
                                     r.art_id as old_art_id,
                                     r.neg_id as neg_id,
                                     to_char(r.data, 'DDMMYYYY') as data,
                                     0 as promo_id,
                                     r.tipo_riga_id as tipo_riga_id,
                                     decode(r.fs_flag, '1', r.qta, 0) as fs_qta,
                                     decode(r.fs_flag, '1', r.imp_netto, 0) as fs_imp,
                                     decode(r.fs_flag, '1', r.fs_sconto, 0) as fs_sconto,
                                     decode(r.fs_flag, '1', r.peso, 0) as fs_peso,
                                     decode(r.fs_flag, '1', r.fs_sconto_trx, 0) as fs_sconto_trx,
                                     decode(r.fs_flag, '1', 0, r.qta) as nfs_qta,
                                     decode(r.fs_flag, '1', 0, r.imp_netto) as nfs_imp,
                                     decode(r.fs_flag, '1', 0, r.nfs_sconto) as nfs_sconto,
                                     decode(r.fs_flag, '1', 0, r.peso) as nfs_peso,
                                     decode(r.fs_flag, '1', 0, r.fs_sconto_trx) as nfs_sconto_trx,
                                     r.bollini as bollini,
                                     r.rowid,
                                     r.cod_ean
                                FROM proc.riga_anomalia r
                               WHERE 1=1
                               and r.flg_verificato='N'
                               and EXISTS
                               (SELECT 1
                                        FROM anag.filtro_negozio fn
                                       WHERE lpad(r.neg_id,4,'0') = lpad(fn.neg_id,4,'0')
                                         AND (fn.filtro_id = 'A' OR
                                              fn.filtro_id = 'P' )))
    loop
      v_cicli:=v_cicli+1;
        begin
          --v_promo:=' ';
          SELECT p.promo_id
          INTO v_promo
          FROM promozione_articolo pa, promozione_negozio pn, promozione p
         WHERE pa.promo_id = p.promo_id
           AND pn.promo_id = p.promo_id
           AND to_date(cur_proc.data, 'DDMMYYYY') between p.data_inizio AND p.data_fine
           AND lpad(pn.neg_id,4,'0') = lpad(cur_proc.neg_id,4,'0')
           AND pa.art_id = cur_proc.art_id;

        exception when no_data_found then
          v_promo:=' ';
        end;

         begin

         select distinct p.art_id
         into v_test_proc
         from proc.articolo_settimana_corrente p
         where lpad(p.neg_id,4,'0')=lpad(cur_proc.neg_id,4,'0')
          and p.data=to_date(cur_proc.data, 'DDMMYYYY')
          and p.art_id=cur_proc.art_id
          and tipo_doc='V';

         update proc.articolo_settimana_corrente u
            set u.promo_id=decode(u.promo_id,null,decode(v_promo,' ',null,v_promo),u.promo_id),
                u.fs_qta=u.fs_qta + cur_proc.fs_qta,
                u.fs_peso=u.fs_peso + cur_proc.fs_peso,
                u.fs_imp=u.fs_imp + cur_proc.fs_imp,
                u.fs_sconto=u.fs_sconto + cur_proc.fs_sconto,
                u.fs_sconto_trx=u.fs_sconto_trx + cur_proc.fs_sconto_trx,
                u.nfs_qta=u.nfs_qta + cur_proc.nfs_qta,
                u.nfs_peso=u.nfs_peso + cur_proc.nfs_peso,
                u.nfs_imp=u.nfs_imp + cur_proc.nfs_imp,
                u.nfs_sconto=u.nfs_sconto + cur_proc.nfs_sconto,
                u.nsf_sconto_trx=u.nsf_sconto_trx + cur_proc.nfs_sconto_trx,
                u.bollini=u.bollini + cur_proc.bollini
          where lpad(u.neg_id,4,'0')=lpad(cur_proc.neg_id,4,'0')
          and u.data=to_date(cur_proc.data, 'DDMMYYYY')
          and u.art_id=cur_proc.art_id
          and u.tipo_doc='V';

         v_count:=v_count+SQL%rowcount;

       exception when no_data_found then

         insert  into proc.articolo_settimana_corrente i
         select cur_proc.neg_id,
                to_date(cur_proc.data,'ddmmyyyy'),
                cur_proc.art_id,
                v_promo,
                cur_proc.fs_qta,
                cur_proc.fs_peso,
                cur_proc.fs_imp,
                cur_proc.fs_sconto,
                cur_proc.nfs_qta,
                cur_proc.nfs_peso,
                cur_proc.nfs_imp,
                cur_proc.nfs_sconto,
                cur_proc.bollini,
                cur_proc.fs_sconto_trx,
                cur_proc.nfs_sconto_trx,
                cur_proc.cod_ean, 
                'V'   --tipo_doc
         from dual;

         v_count_exc:=v_count_exc+SQL%rowcount;
       end;


        update  proc.riga_anomalia a
        set   a.flg_verificato='Y'
        where a.scontrino_prog=cur_proc.scontrino_prog
          and a.art_id=cur_proc.old_art_id
          and a.tipo_riga_id=cur_proc.tipo_riga_id;
    end loop;

    dbms_output.put_line('#cicli proc: '||to_char(v_cicli));
    dbms_output.put_line('#insert proc: '||to_char(v_count_exc));
    dbms_output.put_line('#update proc: '||to_char(v_count));

    DMS_COM.write_jlt(v_unit, 1, '#cicli proc: '||to_char(v_cicli));
    DMS_COM.write_jlt(v_unit, 1, '#insert proc: '||to_char(v_count_exc));
    DMS_COM.write_jlt(v_unit, 1, '#update proc: '||to_char(v_count));

    COMMIT;
   
  /* 02-02-2021 Modifica fatta per aggiungere la gestione delle righe anomalie degli scontrini del reso */
      
    v_cicli:=0;
    v_count_exc:=0;
    v_count:=0;

    for cur_proc_reso in (SELECT distinct r.scontrino_prog as scontrino_prog,
                                     'AA' || lpad(r.rep_pos_id, 4, '0') as art_id,
                                     r.art_id as old_art_id,
                                     r.neg_id as neg_id,
                                     to_char(r.data, 'DDMMYYYY') as data,
                                     0 as promo_id,
                                     r.tipo_riga_id as tipo_riga_id,
                                     --decode(r.fs_flag, '1', r.qta, 0) as fs_qta,
                                     ---decode(r.fs_flag, '1', r.imp_netto, 0) as fs_imp,
                                     ---decode(r.fs_flag, '1', r.fs_sconto, 0) as fs_sconto,
                                     ---decode(r.fs_flag, '1', r.peso, 0) as fs_peso,
                                     ---decode(r.fs_flag, '1', r.fs_sconto_trx, 0) as fs_sconto_trx,
                                     -1 * r.qta as nfs_qta,  ---decode(r.fs_flag, '1', 0, r.qta) as nfs_qta,
                                     -1 * r.imp_netto as nfs_imp, ---decode(r.fs_flag, '1', 0, r.imp_netto) as nfs_imp,
                                     ---decode(r.fs_flag, '1', 0, r.nfs_sconto) as nfs_sconto,
                                     -1 * r.peso as nfs_peso, ---decode(r.fs_flag, '1', 0, r.peso) as nfs_peso,
                                     ---decode(r.fs_flag, '1', 0, r.fs_sconto_trx) as nfs_sconto_trx,
                                     --r.bollini as bollini,
                                     r.rowid
                                     ---r.cod_ean
                                FROM proc.reso_riga_anomalia r
                               WHERE 1=1
                               and r.flg_verificato='N'
                               and EXISTS
                               (SELECT 1
                                        FROM anag.filtro_negozio fn
                                       WHERE lpad(r.neg_id,4,'0') = lpad(fn.neg_id,4,'0')
                                         AND (fn.filtro_id = 'A' OR
                                              fn.filtro_id = 'P' )))
    loop
      v_cicli:=v_cicli+1;
        begin
          --v_promo:=' ';
          SELECT p.promo_id
          INTO v_promo
          FROM promozione_articolo pa, promozione_negozio pn, promozione p
         WHERE pa.promo_id = p.promo_id
           AND pn.promo_id = p.promo_id
           AND to_date(cur_proc_reso.data, 'DDMMYYYY') between p.data_inizio AND p.data_fine
           AND lpad(pn.neg_id,4,'0') = lpad(cur_proc_reso.neg_id,4,'0')
           AND pa.art_id = cur_proc_reso.art_id;

        exception when no_data_found then
          v_promo:=' ';
        end;

         begin

         select distinct p.art_id
         into v_test_proc
         from proc.articolo_settimana_corrente p
         where lpad(p.neg_id,4,'0')=lpad(cur_proc_reso.neg_id,4,'0')
          and p.data=to_date(cur_proc_reso.data, 'DDMMYYYY')
          and p.art_id=cur_proc_reso.art_id
          and tipo_doc='R';

         update proc.articolo_settimana_corrente u
            set u.promo_id=decode(u.promo_id,null,decode(v_promo,' ',null,v_promo),u.promo_id),
                --u.fs_qta=u.fs_qta + cur_proc_reso.fs_qta,
                --u.fs_peso=u.fs_peso + cur_proc_reso.fs_peso,
                --u.fs_imp=u.fs_imp + cur_proc_reso.fs_imp,
                --u.fs_sconto=u.fs_sconto + cur_proc_reso.fs_sconto,
                --u.fs_sconto_trx=u.fs_sconto_trx + cur_proc_reso.fs_sconto_trx,
                u.nfs_qta= u.nfs_qta + cur_proc_reso.nfs_qta,
                u.nfs_peso= u.nfs_peso + cur_proc_reso.nfs_peso,
                u.nfs_imp=u.nfs_imp + cur_proc_reso.nfs_imp --,
                --u.nfs_sconto= u.nfs_sconto + cur_proc_reso.nfs_sconto,
                --u.nsf_sconto_trx=u.nsf_sconto_trx + cur_proc_reso.nfs_sconto_trx,
                --u.bollini=u.bollini + cur_proc_reso.bollini
          where lpad(u.neg_id,4,'0')=lpad(cur_proc_reso.neg_id,4,'0')
          and u.data=to_date(cur_proc_reso.data, 'DDMMYYYY')
          and u.art_id=cur_proc_reso.art_id
          and u.tipo_doc='R';

         v_count:=v_count+SQL%rowcount;

       exception when no_data_found then

         insert  into proc.articolo_settimana_corrente i
         select cur_proc_reso.neg_id,
                to_date(cur_proc_reso.data,'ddmmyyyy'),
                cur_proc_reso.art_id,
                v_promo,
                0, --cur_proc_reso.fs_qta,
                0, --cur_proc_reso.fs_peso,
                0, --cur_proc_reso.fs_imp,
                0 ,-- cur_proc_reso.fs_sconto,
                cur_proc_reso.nfs_qta,
                cur_proc_reso.nfs_peso,
                cur_proc_reso.nfs_imp,
                0 ,-- cur_proc_reso.nfs_sconto,
                0, -- cur_proc_reso.bollini,
                0, --cur_proc_reso.fs_sconto_trx,
                0,-- cur_proc_reso.nfs_sconto_trx,
               '0',-- cur_proc_reso.cod_ean, 
                'R'   --tipo_doc
         from dual;

         v_count_exc:=v_count_exc+SQL%rowcount;
       end;


        update  proc.reso_riga_anomalia a
        set   a.flg_verificato='Y'
        where a.scontrino_prog=cur_proc_reso.scontrino_prog
          and a.art_id=cur_proc_reso.old_art_id
          and a.tipo_riga_id=cur_proc_reso.tipo_riga_id;
    end loop;

    dbms_output.put_line('#cicli proc: '||to_char(v_cicli));
    dbms_output.put_line('#insert proc: '||to_char(v_count_exc));
    dbms_output.put_line('#update proc: '||to_char(v_count));

    DMS_COM.write_jlt(v_unit, 1, '#cicli proc: '||to_char(v_cicli));
    DMS_COM.write_jlt(v_unit, 1, '#insert proc: '||to_char(v_count_exc));
    DMS_COM.write_jlt(v_unit, 1, '#update proc: '||to_char(v_count));

    COMMIT;
 
    

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DWH_PROC_ANOMALIE;

/******************************************************************************
     NAME:       WRAP_STAMPA_EAN_ANOMALIE
     PURPOSE:    Procedura esegue lo script Stampa_ean_anomalie.sh

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        23/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE WRAP_STAMPA_EAN_ANOMALIE
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'WRAP_STAMPA_EAN_ANOMALIE';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_return                NUMBER;

    v_count               NUMBER:=0;
    v_count_exc           NUMBER:=0;
    v_cicli               NUMBER:=0;
    v_found               NUMBER:=0;
    v_exceptions          NUMBER:=0;
    v_promo               VARCHAR2(11);
    v_test                NUMBER;
    v_test_proc           VARCHAR2(16);

    fHandle                 UTL_FILE.FILE_TYPE;
    vcPath                  VARCHAR2(2000);
    vcName                  VARCHAR2(255);
    crlf         CONSTANT VARCHAR2(2) := CHR(13) || CHR(10);

    BEGIN
    dbms_output.enable(40000000);


    select count(*)
      into v_count
      from proc.riga_anomalia r
     where 1=1
     and neg_id in
           (select neg_id from anag.negozio where tipo_neg_id in ('S', 'H', 'Q'))
     and log_date is null;

    if(v_count>0) then

      select  'elenco_anomalie.' ||
              to_char(sysdate, 'yyyymmdd')||
              '.log'
      into vcName
      from dual;

      --impostiamo un reference al file di testo dove scriveremo il body della mail
      fHandle := utl_file.fopen('LOG_DIR', vcName, 'a'); --in append mode...


      for cur in (
                  select art_id,
                         rep_pos_id,
                         neg_id as min_neg_id,
                         max(data) as data,
                         count(*) as conteggio,
                         sum(imp_netto) as importo
                    from proc.riga_anomalia
                   where 1=1
                   and neg_id in
                         (select neg_id from negozio where tipo_neg_id in ('S', 'H', 'Q'))
                   and log_date is null
                   group by art_id, rep_pos_id, neg_id
                   order by art_id, rep_pos_id, neg_id
                   )
      loop

        --richiesto senza intestazioni
        --if(mod(v_cicli,10)=0) then
           --utl_file.put_line(fHandle,'       ART_ID  REP  PDV                  DATA   COUNT(*) SUM(IMP_NETTO)');
           --utl_file.put_line(fHandle,'------------- ---- ---- --------------------- ---------- --------------');
        --end if;
        v_cicli:=v_cicli+1;
           utl_file.put_line(fHandle, lpad(cur.art_id,13,' ')||
                                                    lpad(cur.rep_pos_id,5,' ')||
                                                         lpad(cur.min_neg_id,5,' ')||
                                                              lpad(cur.data,22,' ')||
                                                                                    lpad(cur.conteggio,11,' ')||
                                                                                               lpad(cur.importo,15,' '));

      end loop;

      update proc.riga_anomalia
      set log_date=sysdate
       where 1=1
       --and neg_id in
       --      (select neg_id from negozio where tipo_neg_id in ('S', 'H', 'Q'))
       and log_date is null;


      --pulizia storico <sysdate-7
      delete proc.riga_anomalia
      where log_date <(sysdate-7);


           utl_file.fflush(fHandle); --effettua il flushing del buffer
           utl_file.fclose(fHandle); --chiudiamo il reference al file

           dbms_output.put_line('file di log prodotto nella cartella LOG_DIR');
           dbms_output.put_line('righe di log scritte: '||v_cicli);

           DMS_COM.write_jlt(v_unit, 1, 'file di log prodotto nella cartella LOG_DIR');
           DMS_COM.write_jlt(v_unit, 1, 'righe di log scritte: '||v_cicli);

      COMMIT;
   end if;

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END WRAP_STAMPA_EAN_ANOMALIE;

/******************************************************************************
     NAME:       WRAP_DAILY_SH
     PURPOSE:    Procedura esegue lo script Daily.sh

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        27/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE WRAP_DAILY_SH
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'WRAP_DAILY_SH';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_return                NUMBER;

  BEGIN
    dbms_output.enable(40000000);


    v_command       := '/bin/ksh /dbdwh2/DW/applicativo/ctl/carica_DWHPROD.ksh /export/home/unesdw/script/Daily.sh > /export/home/unesdw/log/Daily.log';
    v_return := DWMSPKG.os_command(v_command);

    DMS_COM.write_jlt(v_unit, 1, v_command);

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END WRAP_DAILY_SH;

/******************************************************************************
     NAME:       WRAP_TUESDAY_SH
     PURPOSE:    Procedura esegue lo script Tuesday.sh

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        27/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE WRAP_TUESDAY_SH
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'WRAP_TUESDAY_SH';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_return                NUMBER;

  BEGIN
    dbms_output.enable(40000000);


    v_command       := '/bin/ksh /dbdwh2/DW/applicativo/ctl/carica_DWHPROD.ksh /export/home/unesdw/script/Tuesday.sh > /export/home/unesdw/log/Tuesday.log';
    v_return := DWMSPKG.os_command(v_command);

    DMS_COM.write_jlt(v_unit, 1, v_command);

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END WRAP_TUESDAY_SH;


/******************************************************************************
     NAME:       WRAP_MONTHLY1_SH
     PURPOSE:    Procedura esegue lo script Monthly1.sh

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        27/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE WRAP_MONTHLY1_SH
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'WRAP_MONTHLY1_SH';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_return                NUMBER;

  BEGIN
    dbms_output.enable(40000000);


    v_command       := '/bin/ksh /dbdwh2/DW/applicativo/ctl/carica_DWHPROD.ksh /export/home/unesdw/script/Monthly1.sh > /export/home/unesdw/log/Monthly1.log';
    v_return := DWMSPKG.os_command(v_command);

    DMS_COM.write_jlt(v_unit, 1, v_command);

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END WRAP_MONTHLY1_SH;

/******************************************************************************
     NAME:       WRAP_MONTHLY2_SH
     PURPOSE:    Procedura esegue lo script Monthly2.sh

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        27/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE WRAP_MONTHLY2_SH
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'WRAP_MONTHLY2_SH';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_return                NUMBER;

  BEGIN
    dbms_output.enable(40000000);


    v_command       := '/bin/ksh /dbdwh2/DW/applicativo/ctl/carica_DWHPROD.ksh /export/home/unesdw/script/Monthly2.sh > /export/home/unesdw/log/Monthly2.log';
    v_return := DWMSPKG.os_command(v_command);

    DMS_COM.write_jlt(v_unit, 1, v_command);

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END WRAP_MONTHLY2_SH;

/******************************************************************************
     NAME:       WRAP_MONTHLY3_SH
     PURPOSE:    Procedura esegue lo script Monthly3.sh

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        27/12/2011  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE WRAP_MONTHLY3_SH
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'WRAP_MONTHLY3_SH';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_return                NUMBER;

  BEGIN
    dbms_output.enable(40000000);


    v_command       := '/bin/ksh /dbdwh2/DW/applicativo/ctl/carica_DWHPROD.ksh /export/home/unesdw/script/Monthly3.sh > /export/home/unesdw/log/Monthly3.log';
    v_return := DWMSPKG.os_command(v_command);

    DMS_COM.write_jlt(v_unit, 1, v_command);


  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END WRAP_MONTHLY3_SH;

/******************************************************************************
     NAME:       DWMS_PROC_INIT
     PURPOSE:    Procedura principale per gli scontrini

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        23/01/2012  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE DWMS_PROC_INIT
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'DWMS_PROC_INIT';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_return                NUMBER;

  BEGIN
    DMS_COM.write_jlt(v_unit, 1, 'Inizio job di inizializzazione scn');
    dbms_output.enable(40000000);

    v_command       := '/bin/ksh /dbdwh2/DW/applicativo/ctl/carica_DWHPROD.ksh /export/home/unesdw/script/load_init.sh > /export/home/unesdw/log/Load_init.log';
    v_return := DWMSPKG.os_command(v_command);

    DMS_COM.write_jlt(v_unit, 1, v_command);

    DMS_COM.write_jlt(v_unit, 1, 'Fine job di inizializzazione scn');

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DWMS_PROC_INIT;

/******************************************************************************
     NAME:       DWMS_PROC_END
     PURPOSE:    Procedura principale per gli scontrini

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        23/01/2012  Massimiliano Modena - Softquattro
  *******************************************************************************/
  PROCEDURE DWMS_PROC_END
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'DWMS_PROC_END';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    r_env                 dms_com_cfg_environment%ROWTYPE;
    v_command             VARCHAR2(1000);
    v_return                NUMBER;

  BEGIN
    DMS_COM.write_jlt(v_unit, 1, 'Inizio job di inizializzazione scn');
    dbms_output.enable(40000000);

    --v_command       := '/bin/ksh /dbdwh2/DW/applicativo/ctl/carica_DWHPROD.ksh /export/home/unesdw/script/load_end.sh';
    v_command       := '/bin/ksh /dbdwh2/DW/applicativo/ctl/carica_DWHPROD.ksh /export/home/unesdw/script/load_end.sh > /export/home/unesdw/log/Load_end.log';
    v_return := DWMSPKG.os_command(v_command);

    DMS_COM.write_jlt(v_unit, 1, v_command);

    DMS_COM.write_jlt(v_unit, 1, 'Fine job di inizializzazione scn');

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' ||
                         SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DWMS_PROC_END;
  
 /******************************************************************************
     NAME:       disableScnProva
     PURPOSE:    Funzione che disabilita il flag per gli eventuali scontrini di prova inviati da NTT all'apertura di un nuovo pdv su 4STORE

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        21/01/2021  Massimiliano Modena - Binnovation
  *******************************************************************************/
  
 FUNCTION disableScnProva RETURN NUMBER IS
    v_unit                  VARCHAR2(30) := 'disableScnProva';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    v_return                VARCHAR2(2):='OK';
    v_return_cmd            VARCHAR2(2);
    v_offset                NUMBER:=5/24;
    n_num_righe             number:=0;
 BEGIN 
   
   DMS_COM.write_jlt(v_unit, 1, 'Inizio modulo '||v_unit);
   
   for c0 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,
                     distinct t.dt_scontrino,t.cod_insegna,t.cod_pdv,t.cod_cassa,t.cod_scontrino 
                from gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where 1=1
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_TESTATA@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c0.dt_scontrino
          and f.cod_insegna  =c0.cod_insegna
          and f.cod_pdv      =c0.cod_pdv
          and f.cod_cassa    =c0.cod_cassa
          and f.cod_scontrino=c0.cod_scontrino;

       n_num_righe := n_num_righe + SQL%rowcount;
    end loop;
    
    for c1 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_TESTATA_JRET@dcg_prod       f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_TESTATA_JRET@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino=c1.dt_scontrino
          and f.cod_insegna=c1.cod_insegna
          and f.cod_pdv=c1.cod_pdv
          and f.cod_cassa=c1.cod_cassa
          and f.cod_scontrino=c1.cod_scontrino;
    end loop;
   
   for c2 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_RIGHE@dcg_prod              f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_RIGHE@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino=c2.dt_scontrino
          and f.cod_insegna=c2.cod_insegna
          and f.cod_pdv=c2.cod_pdv
          and f.cod_cassa=c2.cod_cassa
          and f.cod_scontrino=c2.cod_scontrino;
    end loop;
   
   for c3 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_RIGHE_JRET@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_RIGHE_JRET@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c3.dt_scontrino
          and f.cod_insegna  =c3.cod_insegna
          and f.cod_pdv      =c3.cod_pdv
          and f.cod_cassa    =c3.cod_cassa
          and f.cod_scontrino=c3.cod_scontrino;
    end loop;

   for c4 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_FP@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_FP@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c4.dt_scontrino
          and f.cod_insegna  =c4.cod_insegna
          and f.cod_pdv      =c4.cod_pdv
          and f.cod_cassa    =c4.cod_cassa
          and f.cod_scontrino=c4.cod_scontrino;
    end loop;
    
    for c5 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_FP_JRET@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_FP_JRET@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c5.dt_scontrino
          and f.cod_insegna  =c5.cod_insegna
          and f.cod_pdv      =c5.cod_pdv
          and f.cod_cassa    =c5.cod_cassa
          and f.cod_scontrino=c5.cod_scontrino;
    end loop;
    
    for c6 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_IVA@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_IVA@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c6.dt_scontrino
          and f.cod_insegna  =c6.cod_insegna
          and f.cod_pdv      =c6.cod_pdv
          and f.cod_cassa    =c6.cod_cassa
          and f.cod_scontrino=c6.cod_scontrino;
    end loop;
    
    for c7 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_TESTATA_CARD@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_TESTATA_CARD@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c7.dt_scontrino
          and f.cod_insegna  =c7.cod_insegna
          and f.cod_pdv      =c7.cod_pdv
          and f.cod_cassa    =c7.cod_cassa
          and f.cod_scontrino=c7.cod_scontrino;
    end loop;
    
    for c8 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_TESTATA_CARD_JRET@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_TESTATA_CARD_JRET@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c8.dt_scontrino
          and f.cod_insegna  =c8.cod_insegna
          and f.cod_pdv      =c8.cod_pdv
          and f.cod_cassa    =c8.cod_cassa
          and f.cod_scontrino=c8.cod_scontrino;
    end loop;
    
    for c9 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_DETT_CARD@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_DETT_CARD@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c9.dt_scontrino
          and f.cod_insegna  =c9.cod_insegna
          and f.cod_pdv      =c9.cod_pdv
          and f.cod_cassa    =c9.cod_cassa
          and f.cod_scontrino=c9.cod_scontrino;
    end loop;
    
    for c10 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_PROMO@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_PROMO@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c10.dt_scontrino
          and f.cod_insegna  =c10.cod_insegna
          and f.cod_pdv      =c10.cod_pdv
          and f.cod_cassa    =c10.cod_cassa
          and f.cod_scontrino=c10.cod_scontrino;
    end loop;
    
    for c11 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_PROMO_JRET@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_PROMO_JRET@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c11.dt_scontrino
          and f.cod_insegna  =c11.cod_insegna
          and f.cod_pdv      =c11.cod_pdv
          and f.cod_cassa    =c11.cod_cassa
          and f.cod_scontrino=c11.cod_scontrino;
    end loop;
    
    for c12 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_ART_VIRTUALI@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_ART_VIRTUALI@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c12.dt_scontrino
          and f.cod_insegna  =c12.cod_insegna
          and f.cod_pdv      =c12.cod_pdv
          and f.cod_cassa    =c12.cod_cassa
          and f.cod_scontrino=c12.cod_scontrino;
    end loop;
    
    for c13 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_ART_VIRTUALI_JRET@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_ART_VIRTUALI_JRET@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c13.dt_scontrino
          and f.cod_insegna  =c13.cod_insegna
          and f.cod_pdv      =c13.cod_pdv
          and f.cod_cassa    =c13.cod_cassa
          and f.cod_scontrino=c13.cod_scontrino;
    end loop;
    
    for c14 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_AUTORIZZ_SV@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_AUTORIZZ_SV@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c14.dt_scontrino
          and f.cod_insegna  =c14.cod_insegna
          and f.cod_pdv      =c14.cod_pdv
          and f.cod_cassa    =c14.cod_cassa
          and f.cod_scontrino=c14.cod_scontrino;
    end loop;
    
    for c15 in (select --t.dt_scontrino,t.ora_fine,t.cod_pdv,t.cod_cassa,t.cod_scontrino,t.dt_scontrino ||' '|| t.ora_fine,f.* 
                     distinct f.dt_scontrino,f.cod_insegna,f.cod_pdv,f.cod_cassa,f.cod_scontrino 
                from gefa.JSON_SCN_EAN_NON_CASSA@dcg_prod         f,
                     gefa.JSON_SCN_TESTATA@dcg_prod            t,
                     etl.dcg_an_pdv@dcg_prod                   a 
               where f.dt_scontrino=t.dt_scontrino
                 and f.cod_insegna=t.cod_insegna
                 and f.cod_pdv=t.cod_pdv
                 and f.cod_cassa=t.cod_cassa
                 and f.cod_scontrino=t.cod_scontrino
                 and 'E_'||case when length(to_char(t.COD_PDV))=4 then to_char(t.COD_PDV) 
                           else lpad(t.COD_PDV,3,'0') 
                           end = a.cd_entity
                 and ((a.FLG_4S='Y' and to_date(t.dt_scontrino ||' '|| t.ora_fine ,'yyyy-mm-dd hh24:mi:ss') < DT_AVVIO_4S +5/24) or NVL(a.FLG_4S,'N')='N')
                 and t.flg_elaborazione='W'
                 ) 
    loop
       update gefa.JSON_SCN_EAN_NON_CASSA@dcg_prod            f 
          set f.flg_elaborazione='A' 
        where f.dt_scontrino =c15.dt_scontrino
          and f.cod_insegna  =c15.cod_insegna
          and f.cod_pdv      =c15.cod_pdv
          and f.cod_cassa    =c15.cod_cassa
          and f.cod_scontrino=c15.cod_scontrino;
    end loop;
    
    commit;
    DMS_COM.write_jlt(v_unit, 1, 'Effettuata sincronizzazione ''Anomalia'' '||to_char(n_num_righe)||' scontrini sulle sorgenti con pdv non in anagrafica');
    
    DMS_COM.write_jlt(v_unit, 1, 'Fine modulo '||v_unit);
    
    RETURN 1; --OK

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RETURN 0; --KO
  END disableScnProva;
  
  /******************************************************************************
     NAME:       DWMS_PROC_CHECK_RESI
     PURPOSE:    Procedura che effettua i controlli di consistenza dei resi
                 inviati da 4STORE

     REVISIONS:
     Ver        Date        Author              Description
     ---------  ----------  ---------------     ---------------------------------
     1.0        10/03/2021  Massimiliano Modena - Binnovation
  *******************************************************************************/
  PROCEDURE DWMS_PROC_CHECK_RESI
  (
    p_job    IN VARCHAR2
   ,p_period IN VARCHAR2
   ,p3       IN VARCHAR2 DEFAULT NULL
   ,p4       IN VARCHAR2 DEFAULT NULL
   ,p5       IN VARCHAR2 DEFAULT NULL
   ,p6       IN VARCHAR2 DEFAULT NULL
   ,p7       IN VARCHAR2 DEFAULT NULL
   ,p8       IN VARCHAR2 DEFAULT NULL
   ,p9       IN VARCHAR2 DEFAULT NULL
   ,p10      IN VARCHAR2 DEFAULT NULL
   ,p11      IN VARCHAR2 DEFAULT NULL
  ) IS

    v_unit VARCHAR2(30) := 'DWMS_PROC_CHECK_RESI';
    k_error_marker CONSTANT CHAR(22) := '[*** !!! ***]  [ERROR]';
    n_num_righe           NUMBER;
    --variabili per creazione ed invio MAIL
    PID             NUMBER;
    NUM_GRUPPO_DEST VARCHAR2(10) := '102';  
    v_testo         VARCHAR2(4000);
    crlf   CONSTANT VARCHAR2(2) := CHR(13) || CHR(10);
    v_offset        NUMBER:=6;


  BEGIN
  
       /*************************************************************************************/
       /***                      INVIO MAIL DI controllo                                  ***/
       /************************************************ ************************************/

       PID := null;                                              -- inizializzazione reference della mail
       v_Testo := ' ';
       PID     := TESTO_MAIL@dcg_prod(v_Testo, PID);             -- creazione reference al testo della mail
       
       V_Testo := 'Data di Competenza;Cod Pdv;Importo Reso FC;Importo Scn testata;Importo Scn FP;Importo Scn dettagli;Esito Check';
       Pid     := Testo_Mail@dcg_prod(V_Testo, Pid);
       V_Testo := '------------------;-------;---------------;-------------------;--------------;--------------------;------------';
       Pid     := Testo_Mail@dcg_prod(V_Testo, Pid);

  for c in (select lpad(to_char(dt_competenza,'yyyy-mm-dd'),18) as dt_competenza,
                   lpad(cod_pdv,7) as cod_pdv,
                   lpad(nvl(to_char(im_resi_tot),' '),15)as im_resi_tot,
                   lpad(nvl(to_char(importo_3),' '),19) as importo_3,
                   lpad(nvl(to_char(importo_4),' '),14)as importo_4,
                   lpad(nvl(to_char(importo_5),' '),20)as importo_5,
                   case when nvl(im_resi_tot,0)!= nvl(importo_3,0) or nvl(im_resi_tot,0)!= nvl(importo_4,0) or nvl(im_resi_tot,0)!= nvl(importo_5,0) then 'DA VERIFICARE'
                     else '          OK'
                   end as ESITO
                   from (select dt_competenza,
                                cod_pdv,
                                -sum(im_resi_tot) im_resi_tot,
                                sum(importo_3)    importo_3,
                                sum(importo_4)    importo_4,
                                sum(importo_5)    importo_5
                          from (select * from ( select to_date(t.dt_vendita,'yyyy-mm-dd') as dt_competenza,
                                                       lpad(t.cod_pdv,3,'0') as cod_pdv,
                                                       sum(d.im_resi_tot) as im_resi_tot,
                                                       null as importo_3,
                                                       null as importo_4,
                                                       null as importo_5
                                                 from  gefa.json_fc_dett_eod_jret@dcg_prod d,
                                                       gefa.json_fc_testata_eod@dcg_prod t
                                                 where t.cod_finegiorno=d.cod_finegiorno
                                                   and t.cod_pdv=d.cod_pdv
                                                   and t.cod_insegna=d.cod_insegna
                                                group by  t.dt_vendita,t.cod_finegiorno,t.cod_pdv  
                                                order by  t.dt_vendita desc,t.cod_finegiorno desc,t.cod_pdv)
                                              where im_resi_tot!=0
                                union all
                                select  dt_competenza,
                                               cod_pdv,
                                               null as im_resi_tot,
                                               sum(importo_3) importo_3,
                                               sum(importo_4) importo_4,
                                               sum(importo_5) importo_5
                                  from (select dt_competenza,
                                               cod_pdv,
                                               case when tipo_record=3 then sum(tot_netto) else null
                                                end as importo_3,
                                               case when tipo_record=4 then sum(valore_netto) else null
                                                end as importo_4,
                                               case when tipo_record=5 then sum(valore_netto_riga) else null
                                                end as importo_5
                                          from dwh_dati_scn_stor t
                                        where dt_competenza>=sysdate-v_offset
                                        and tipo_record in (3,4,5)
                                        --and cod_pdv in (select distinct lpad(cod_pdv,3,'0') as cod_pdv from gefa.json_fc_dett_eod_jret@dcg_prod)
                                        and (cod_pdv,dt_competenza) in (select distinct lpad(cod_pdv,3,'0') as cod_pdv,to_date(dt_scontrino,'yyyy-mm-dd') from gefa.JSON_FC_TESTATA@dcg_prod)
                                        group by dt_competenza,
                                                 cod_pdv,
                                                 tipo_record
                                        order by dt_competenza,
                                                 cod_pdv)              
                                group by dt_competenza,
                                                 cod_pdv)
                   group by dt_competenza,
                            cod_pdv) 
            where dt_competenza> sysdate-v_offset
            order by 1 desc)
  loop
      
               V_Testo := c.dt_competenza||';'||c.cod_pdv||';'||c.im_resi_tot||';'||c.importo_3||';'||c.importo_4||';'||c.importo_5||';'||c.esito;
               Pid     := Testo_Mail@dcg_prod(V_Testo, Pid);
               
               
               
  end loop;
      
               send_mail@dcg_prod('Report verifica scontrini dei resi '||to_char(sysdate,'dd/mm/yyyy'),PID,NUM_GRUPPO_DEST);

  EXCEPTION
    WHEN OTHERS THEN
      DMS_COM.write_jlt(v_unit, 1, k_Error_Marker);
      DMS_COM.write_jlt(v_unit, 1, 'Generata eccezione ' || SQLCODE || ' ' || SQLERRM);
      DMS_LAT.elab_fail(SQLERRM);
      RAISE;
  END DWMS_PROC_CHECK_RESI;

END DWH_PROC;
/
