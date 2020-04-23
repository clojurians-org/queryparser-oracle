  CREATE OR REPLACE PROCEDURE "FSD"."PR_GL_ACCT_ALL" 
(   P_DATA_DATE   IN     VARCHAR2,
    P_O_RESULT    OUT    VARCHAR2,
    P_LAST_DATE   IN     VARCHAR2  DEFAULT NULL)
IS
   /******************************************************************************
     AUTHOR : wang.lei
     NAME   : PR_GL_ACCT
     FUNCTIONS : 总账帐户信息表
     PURPOSE   :
     INTABLE  : FSD.GL_CODE_DEF, FSD.GL_ACCT_ALL_HIST, ODS.SYM_GL_ACCT, ODS.SYM_FM_GL_MAST_TBL ,FSD.VW_BOCZ_GL_BAL, FSD.GL_POST
     OUTTABLE : FSD.GL_ACCT_ALL
     ARGS     : 参数说明
               P_DATA_DATE：输入业务日期
               P_O_RESULT ：运行结果 UTIL.SUCCESS 成功， UTIL.FAILED 失败
               P_LAST_DATE: 上次批处理的日期
     REVISIONS OR COMMENTS
     VER        DATE          AUTHOR           DESCRIPTION
   ---------  ----------  ---------------  ------------------------------------
     1.0        2016-9-6      xxx         1. CREATED THIS PACKAGE.
  ******************************************************************************/
   V_STEP              VARCHAR2 (10 CHAR) := '0';
   V_PROC_NAME         VARCHAR2 (100 CHAR) := 'PR_GL_ACCT_ALL';
   V_TABLE_NAME        VARCHAR2 (30 CHAR) := 'GL_ACCT_ALL';
   V_TABLE_NAME_HIST   VARCHAR2 (30 CHAR) := 'GL_ACCT_ALL' || '_HIST';
   V_DATA_DATE         VARCHAR2 (8 CHAR);
   V_SUCCESS           VARCHAR2 (10 CHAR) := COMM.UTIL.SUCCESS;
   V_FAILED            VARCHAR2 (10 CHAR) := COMM.UTIL.FAILED;
   V_EDATE             VARCHAR2 (10 CHAR) := COMM.UTIL.EDATE;
   V_START_TIME        VARCHAR2 (100 CHAR);
   V_END_TIME          VARCHAR2 (100 CHAR);
   V_CNT               NUMBER DEFAULT 0 ;
   V_YESTODAY          VARCHAR2 (8 CHAR);
-------------------------------------------------------

BEGIN
   /*INIT*/
   V_STEP := '1';
   V_PROC_NAME := 'PR_GL_ACCT_ALL';
   V_TABLE_NAME := 'GL_ACCT_ALL';
   V_TABLE_NAME_HIST := 'GL_ACCT_ALL' || '_HIST';
   P_O_RESULT := V_SUCCESS;
   V_DATA_DATE := P_DATA_DATE;
   V_START_TIME := TO_CHAR (SYSTIMESTAMP, 'YYYY-MM-DD HH24:MI:SS.FF');

   V_STEP := '1.1';
   --昨日
   --未了支持跨日批处理执行，昨日则为系统的是那个一个运行日期
   IF P_LAST_DATE IS NULL
      THEN
         V_YESTODAY := COMM.UTIL.GET_LAST_RUN_DATE;
      ELSE
         V_YESTODAY := P_LAST_DATE;
   END IF;

   --如果是系统上线日期，则清除数据表数据
   V_STEP := '2';
   IF V_DATA_DATE = COMM.UTIL.GET_SYSTEM_START_DATE
      THEN
          --清除历史表数据
          EXECUTE IMMEDIATE   'TRUNCATE TABLE FSD.'
                           || V_TABLE_NAME
                           ||'_HIST'
                           || ' DROP STORAGE ';
   END IF;

   V_STEP := '2.1';
   --清除当前数据表
   EXECUTE IMMEDIATE   'TRUNCATE TABLE FSD.'
                        || V_TABLE_NAME
                        || ' DROP STORAGE ';
   --模型数据加载
   V_STEP := '3';
   IF V_DATA_DATE = COMM.UTIL.GET_SYSTEM_START_DATE
      THEN
       --上线日取得财务系统的科目余额
       V_STEP := '3.1';
       INSERT INTO FSD.GL_ACCT_ALL (
              SDATE,
              EDATE,
              BRANCH,
              CCY,
              GL_CODE,
              ACTUAL_BAL,
              BALANCE_DIR )
       SELECT V_DATA_DATE,
              V_EDATE,
              '10001'  BRANCH,
              'CNY'    CCY,
              FACCOUNT   GL_CODE,
              SUM(DECODE(FDC,'jie',NVL(FDEBITENDFOR,0),'dai',NVL(FCREDITENDFOR,0))) ACTUAL_BAL,
              NVL(DECODE(FDC,'jie','D','dai','C'),GC.BALANCE_DIR)
         FROM FSD.VW_BOCZ_GL_BAL GA,
              FSD.GL_CODE_DEF GC
        WHERE GA.FACCOUNT = GC.GL_CODE
          AND GA.SDATE = V_DATA_DATE
          AND (FDEBITENDFOR IS NOT NULL OR FCREDITENDFOR IS NOT NULL)
          AND V_DATA_DATE BETWEEN GC.SDATE AND GC.EDATE
        GROUP BY '10001',
                 FACCOUNT,
                 'CNY',
                 NVL(DECODE(FDC,'jie','D','dai','C'),GC.BALANCE_DIR)
        UNION ALL
       --取得核心系统的科目余额
       SELECT V_DATA_DATE,
              V_EDATE,
              GA.BRANCH,
              GA.CCY,
              GA.GL_CODE,
              SUM(DECODE(BALANCE_WAY,'D',ACTUAL_BAL,'C',-1*ACTUAL_BAL,'A',DECODE(SIGN(ACTUAL_BAL),-1,-1*ACTUAL_BAL,ACTUAL_BAL),0)),
              DECODE(BALANCE_WAY,'A',DECODE(SIGN(ACTUAL_BAL),1,'D','C'),BALANCE_WAY)
         FROM ODS.SYM_GL_ACCT GA,
              ODS.SYM_FM_GL_MAST_TBL GMT
        WHERE GA.GL_CODE = GMT.GL_CODE
          AND GA.SDATE=V_DATA_DATE
          AND (V_DATA_DATE BETWEEN GMT.SDATE AND GMT.EDATE)
        GROUP BY GA.BRANCH,
                 GA.CCY,
                 GA.GL_CODE,
                 DECODE(BALANCE_WAY,'A',DECODE(SIGN(ACTUAL_BAL),1,'D','C'),BALANCE_WAY);
       COMMIT;
   ELSE
       --得到财务系统昨日余额
       V_STEP := '3.1';
       INSERT INTO FSD.GL_ACCT_ALL (
              SDATE,
              EDATE,
              BRANCH,
              CCY,
              GL_CODE,
              ACTUAL_BAL,
              BALANCE_DIR)
       SELECT V_DATA_DATE,
              EDATE,
              BRANCH,
              CCY,
              GL_CODE,
              ACTUAL_BAL,
              BALANCE_DIR
         FROM FSD.GL_ACCT_ALL_HIST GA
        WHERE GA.SDATE = V_YESTODAY
          AND BRANCH='10001';
       COMMIT;
       --得到核心新增科目余额
       V_STEP := '3.2';
       INSERT INTO FSD.GL_ACCT_ALL (
              SDATE,
              EDATE,
              BRANCH,
              CCY,
              GL_CODE,
              ACTUAL_BAL,
              BALANCE_DIR)
       SELECT DISTINCT
              V_DATA_DATE,
              V_EDATE,
              GA.BRANCH,
              GA.CCY,
              GA.GL_CODE,
              SUM(DECODE(BALANCE_WAY,'D',ACTUAL_BAL,'C',-1*ACTUAL_BAL,'A',DECODE(SIGN(ACTUAL_BAL),-1,-1*ACTUAL_BAL,ACTUAL_BAL),0)),
              DECODE(BALANCE_WAY,'A',DECODE(SIGN(ACTUAL_BAL),1,'D','C'),BALANCE_WAY)
         FROM ODS.SYM_GL_ACCT GA,
              ODS.SYM_FM_GL_MAST_TBL GMT
        WHERE GA.GL_CODE = GMT.GL_CODE
          AND GA.SDATE=V_DATA_DATE
          AND (V_DATA_DATE BETWEEN GMT.SDATE AND GMT.EDATE)
        GROUP BY GA.BRANCH,
                 GA.CCY,
                 GA.GL_CODE,
                 DECODE(BALANCE_WAY,'A',DECODE(SIGN(ACTUAL_BAL),1,'D','C'),BALANCE_WAY);
       COMMIT;
       --得到财务,债券新增科目
       V_STEP := '3.2.1';

       INSERT INTO FSD.GL_ACCT_ALL (
              SDATE,
              EDATE,
              BRANCH,
              CCY,
              GL_CODE,
              BALANCE_DIR)

              WITH BAL AS (
                           SELECT BRANCH,
                                  CCY,
                                  GL_CODE,
                                  SUM(TRAN_AMT) TRAN_AMT
                             FROM (
                                   SELECT                                            --财务系统
                                         '10001'  BRANCH,
                                         'CNY'       CCY,
                                         FACCOUNT   GL_CODE,
                                         DECODE(FDC,'dai',-1 * FCREDITFOR,FDEBITFOR) TRAN_AMT
                                    FROM FSD.VW_BOCZ_GL_BAL GA
                                   WHERE GA.SDATE = V_DATA_DATE
                                     AND (FDEBITFOR IS NOT NULL OR FCREDITFOR IS NOT NULL)  --有发生额的
                                   /* 配合系统上线，暂时没有CMS外围系统，注释掉，以后添加CMS外围后，除掉注释  2014.11.11 mod start
                                   UNION ALL                                              --债券系统
                                  SELECT
                                         '10001'  BRANCH,
                                         'CNY'       CCY,
                                         ACCOUNTINGCODE   GL_CODE,
                                         AMOUNT TRAN_AMT
                                    FROM TBS_V_ACCENTRY2 GA
                                   WHERE GA.SETTLEDATE = V_DATA_DATE 2014.11.11 mod end */
                                  )
                            GROUP BY BRANCH,CCY,GL_CODE
                          )
       SELECT DISTINCT
              V_DATA_DATE,
              V_EDATE,
              '10001'  BRANCH,
              'CNY'       CCY,
              GA.GL_CODE,
              DECODE(BALANCE_WAY,'A',DECODE(SIGN(TRAN_AMT),1,'D','C'),BALANCE_WAY)
         FROM BAL GA,
              ODS.SYM_FM_GL_MAST_TBL GC
        WHERE GA.GL_CODE = GC.GL_CODE
          AND (V_DATA_DATE BETWEEN GC.SDATE AND GC.EDATE)
          AND NOT EXISTS (SELECT 1 FROM FSD.GL_ACCT_ALL GAA
                           WHERE GAA.CCY = 'CNY'
                             AND GAA.BRANCH='10001'
                             AND GAA.GL_CODE = GA.GL_CODE);
       COMMIT;

       --计算财务，债券当日余额
       V_STEP := '3.2.2';
       MERGE INTO FSD.GL_ACCT_ALL DEST
       USING (
              SELECT '10001' BRANCH,
                     GLP.CCY,
                     GLP.GL_CODE,
                     SUM(TRAN_AMT) TRAN_AMT
                FROM FSD.GL_POST GLP
               WHERE GLP.SDATE = V_DATA_DATE
                 AND SYS_FLAG IN ('GLS','TBS')
               GROUP BY '10001',
                        GLP.CCY,
                        GLP.GL_CODE
              ) SOUR
          ON (DEST.BRANCH = SOUR.BRANCH AND
              DEST.CCY = SOUR.CCY AND
              DEST.GL_CODE = SOUR.GL_CODE)
        WHEN MATCHED THEN
      UPDATE SET DEST.ACTUAL_BAL = DECODE(DEST.BALANCE_DIR,'D',DECODE(DEST.BALANCE_DIR,'D',NVL(DEST.ACTUAL_BAL,0),-1 * NVL(DEST.ACTUAL_BAL,0)) + NVL(SOUR.TRAN_AMT,0),
                                          -1*(DECODE(DEST.BALANCE_DIR,'D',NVL(DEST.ACTUAL_BAL,0),-1 * NVL(DEST.ACTUAL_BAL,0)) + NVL(SOUR.TRAN_AMT,0)))
       WHERE DEST.BRANCH='10001';
    END IF;
   /*将当前数据加载到历史数据区*/
   V_STEP := 'N-1';
   COMM.PACK_COMM.PR_LOAD_DATA (V_DATA_DATE,'FSD',V_TABLE_NAME, P_O_RESULT);

   /*处理结束，记录日志信息*/
   V_STEP := 'N';
   V_END_TIME := TO_CHAR (SYSTIMESTAMP, 'YYYY-MM-DD HH24:MI:SS.FF');
   COMM.ETL.WRITE_TRACE (V_PROC_NAME,V_START_TIME,V_END_TIME,P_O_RESULT,V_CNT);
EXCEPTION
   WHEN OTHERS THEN
      P_O_RESULT := V_FAILED;
      COMM.ETL.WRITE_LOG (V_PROC_NAME,V_STEP,'ERROR OCCUR:' || SQLERRM,P_O_RESULT);
      RAISE_APPLICATION_ERROR (-20001, V_PROC_NAME);
END PR_GL_ACCT_ALL;
