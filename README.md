//+------------------------------------------------------------------+
//| Wyckoff_CADS_NoFVG_EFG_v2_2_modified_fixed.mq5                   |
//| Composite Accumulation/Distribution Score with Direction HUD     |
//| Wyckoff without FVG drawing.                                     |
//| - Compact HUD, EVR rows, Sequencer (SOS/SOW->Test), LPS/LPSY     |
//| - AVWAP from SC/BC                                               |
//| - Simple Long / Hold / Short direction engine + alerts           |
//+------------------------------------------------------------------+
#property strict
#property indicator_chart_window
#property indicator_plots   0
#property indicator_buffers 0

//===================== Inputs ======================================
// HUD
input ENUM_BASE_CORNER HUD_Corner     = CORNER_LEFT_UPPER;
input int              HUD_X          = 8;
input int              HUD_Y          = 22;
input color            HUD_Text       = clrBlack;
input color            HUD_Accent     = clrCrimson;
input color            HUD_Label      = clrBlack;
input int              Heat_Width     = 160;
input int              Heat_Height    = 6;
input int              Heat_Segments  = 12;

// CADS colors
input color            CADS_Color_Accum   = clrLime;
input color            CADS_Color_Neutral = clrYellow;
input color            CADS_Color_Dist    = clrRed;

// Core windows
input int    ATR_Period              = 14;
input int    VolRank_Window          = 80;
input int    Range_Window            = 60;
input int    Swing_Lookback          = 2;

// MTF bias
input bool   Use_MTF_Bias            = true;
input ENUM_TIMEFRAMES BiasTF         = PERIOD_H1;
input int    Bias_EMA_Period         = 34;
input int    Bias_Slope_Bars         = 3;

// Score weights
input double W_Structure             = 0.30;
input double W_EVR                   = 0.35;
input double W_CLV                   = 0.15;
input double W_MTF                   = 0.20;

// Thresholds
input int    CADS_Accum_Threshold    = 68;
input int    CADS_Dist_Threshold     = 32;

// EVR tuning
input double EVR_Absorb_Prox_ATR     = 0.7;
input double EVR_Effort_Z_Min        = 0.50;
input double EVR_Result_MinATR       = 0.40;

// Sequencer
input double Seq_BodyFrac_Min        = 0.50;
input double Seq_VolRank_Min         = 0.60;
input double Seq_Test_VolPct_Max     = 0.60;
input int    Seq_Lookahead_Bars      = 10;

// LPS/LPSY
input bool   LPS_Enable              = true;
input double LPS_TestDepth_ATR       = 0.6;
input int    LPS_MaxBars             = 12;
input double LPS_VolPct_Max          = 0.70;

// AVWAP
input bool   AVWAP_Enable            = true;
input bool   AVWAP_From_SC           = true;
input bool   AVWAP_From_BC           = true;

// Persistence / markers
input bool   Persist_ToCSV           = true;
input bool   Draw_Signal_Markers     = true;
input color  Color_SOS               = clrLime;
input color  Color_SOW               = clrRed;
input color  Color_ZoneChange        = clrDodgerBlue;
input color  Color_LPS               = clrDeepSkyBlue;
input color  Color_LPSY              = clrTomato;

// Performance
input int    CPU_MaxBars_Backfill    = 800;
input int    CPU_Scan_Step           = 1;

// Alerts
input bool   Alerts_Enable           = true;
input bool   Alerts_Popup            = true;
input bool   Alerts_Sound            = true;
input bool   Alerts_Push             = false;
input string Alerts_Sound_File       = "alert.wav";
input int    Alert_Cooldown_Sec      = 600;

// --------- Direction HUD (replaces old EG BLOCK logic) ---------
input bool   EG_Enable               = true;
input int    EG_FontSize             = 9;
input int    EG_Spacing_X            = 15;
input int    EG_OffsetY              = 55;
input color  EG_Green                = clrLime;
input color  EG_Amber                = clrGold;       // kept for compatibility, not used
input color  EG_Red                  = clrTomato;
input color  EG_Text                 = clrBlack;
input double EG_MinVolPercentile     = 0.60;          // min volume percentile for real signals
input color  EG_Hold_Color           = clrSilver;     // HOLD / neutral color

// Liquidity/Efficiency proxy (no FVG drawing)
input int    LQ_EQ_Tolerance_Points  = 2;    // equal highs/lows tolerance (points)
input int    LQ_PoolScan_Bars        = 150;  // window to scan for pools
input int    LQ_MinTouches           = 2;    // equal highs/lows required
input int    LQ_SweepSearch_Bars     = 12;   // bars after pool to look for sweep
input double Eff_CloseInsideFrac     = 0.50; // close back inside fraction of range
input int    Eff_ATR_Window          = 30;   // window for efficiency proxy
input double Eff_Wide_TR_to_ATR      = 0.80; // classify wide bar
input double Eff_MinOverlapRatio     = 0.25; // min overlap to consider bar efficient
input int    Eff_Stacks_DeepRetrace  = 2;    // stacks to consider deep retrace
input ENUM_TIMEFRAMES HTF_for_MacroIneff = PERIOD_H1; // (reserved) for headwind maturity
input int    HTF_Maturity_Bars       = 20;   // (reserved) bars to accept HTF headwind

//===================== IDs / Globals ===============================
#define PFX          "CADS_WY2_"
#define L(n)         PFX n
#define OBJ_TITLE    L("TTL")
#define OBJ_INFO     L("INF")
#define OBJ_SCORE    L("SCO")
#define OBJ_HEAT1    L("H1_")
#define OBJ_HEAT2    L("H2_")
#define OBJ_EVR1LBL  L("LBL_E1")
#define OBJ_EVR2LBL  L("LBL_E2")
#define OBJ_AVWAP    L("AVWAP")

int g_atr = INVALID_HANDLE;
int g_bias_ema = INVALID_HANDLE;
int g_htf_ema = INVALID_HANDLE;

datetime g_live_time=0;
double   g_live_open=0, g_live_high=0, g_live_low=0, g_live_close=0;
long     g_live_vol=0; double g_live_atr=0;

datetime g_lastAlert_CADS=0, g_lastAlert_SOS=0, g_lastAlert_SOW=0, g_lastAlert_EG=0;

int g_anchor_index_SC = -1;
int g_anchor_index_BC = -1;

//===================== Utils ======================================
string TFToStr(ENUM_TIMEFRAMES tf){
   switch(tf){
      case PERIOD_M1:  return "M1"; case PERIOD_M2:  return "M2";  case PERIOD_M3:  return "M3";
      case PERIOD_M4:  return "M4"; case PERIOD_M5:  return "M5";  case PERIOD_M6:  return "M6";
      case PERIOD_M10: return "M10";case PERIOD_M12: return "M12"; case PERIOD_M15: return "M15";
      case PERIOD_M20: return "M20";case PERIOD_M30: return "M30"; case PERIOD_H1:  return "H1";
      case PERIOD_H2:  return "H2"; case PERIOD_H3:  return "H3";  case PERIOD_H4:  return "H4";
      case PERIOD_H6:  return "H6"; case PERIOD_H8:  return "H8";  case PERIOD_H12: return "H12";
      case PERIOD_D1:  return "D1"; case PERIOD_W1:  return "W1";  case PERIOD_MN1: return "MN1";
      default:         return "TF"+IntegerToString((int)tf);
   }
}

void SetSeriesRates(MqlRates &arr[]){ ArraySetAsSeries(arr,true); }
void SetSeriesLong(long &arr[]){ ArraySetAsSeries(arr,true); }
void SetSeriesDouble(double &arr[]){ ArraySetAsSeries(arr,true); }

color RGB3(int r,int g,int b){ return (color)((r&255)|((g&255)<<8)|((b&255)<<16)); }

void ApplyCorner(string name){ ObjectSetInteger(0,name,OBJPROP_CORNER,HUD_Corner); }
void EnsureLabel(string name,string txt,int x,int y,int fs,color clr,bool back=true)
{
   if(ObjectFind(0,name)==-1) ObjectCreate(0,name,OBJ_LABEL,0,0,0);
   ApplyCorner(name);
   ObjectSetInteger(0,name,OBJPROP_XDISTANCE,x);
   ObjectSetInteger(0,name,OBJPROP_YDISTANCE,y);
   ObjectSetInteger(0,name,OBJPROP_FONTSIZE,fs);
   ObjectSetInteger(0,name,OBJPROP_COLOR,clr);
   ObjectSetInteger(0,name,OBJPROP_BACK,back);
   ObjectSetInteger(0,name,OBJPROP_SELECTABLE,false);
   ObjectSetString(0,name,OBJPROP_TEXT,txt);
}
void EnsureRect(string name,int x,int y,int w,int h,color c,bool back=true)
{
   if(ObjectFind(0,name)==-1) ObjectCreate(0,name,OBJ_RECTANGLE_LABEL,0,0,0);
   ApplyCorner(name);
   ObjectSetInteger(0,name,OBJPROP_XDISTANCE,x);
   ObjectSetInteger(0,name,OBJPROP_YDISTANCE,y);
   ObjectSetInteger(0,name,OBJPROP_XSIZE,w);
   ObjectSetInteger(0,name,OBJPROP_YSIZE,h);
   ObjectSetInteger(0,name,OBJPROP_BGCOLOR,c);
   ObjectSetInteger(0,name,OBJPROP_BACK,back);
   ObjectSetInteger(0,name,OBJPROP_COLOR,c);
   ObjectSetInteger(0,name,OBJPROP_SELECTABLE,false);
}
void EnsureHeatbar(const string pref,int x,int y,int w,int h,int segs)
{
   int segw=w/segs; if(segw<2) segw=2;
   for(int i=0;i<segs;i++){
      string nm=pref+IntegerToString(i);
      EnsureRect(nm,x+i*segw,y,segw-1,h,clrDimGray);
   }
}
void ColorHeatbar(const string pref,int segs,double ratio)
{
   if(ratio<0.0) ratio=0.0; if(ratio>1.0) ratio=1.0;
   int active=(int)MathFloor(ratio*segs+0.5);
   for(int i=0;i<segs;i++){
      string nm=pref+IntegerToString(i);
      double t = (segs>1 ? (double)i/(segs-1) : 0.0);
      color c=(t<0.5? RGB3( (int)(t*2.0*255.0),255,0) : RGB3(255,255-(int)((t-0.5)*2.0*255.0),0));
      ObjectSetInteger(0,nm,OBJPROP_BGCOLOR,(i<active?c:clrGray));
   }
}

bool IsSwingHigh(const MqlRates &r[], int i, int lb){
   for(int k=1;k<=lb;k++){ if(i-k<0 || i+k>=ArraySize(r)) return false;
      if(!(r[i].high>r[i-k].high && r[i].high>r[i+k].high)) return false; }
   return true;
}
bool IsSwingLow(const MqlRates &r[], int i, int lb){
   for(int k=1;k<=lb;k++){ if(i-k<0 || i+k>=ArraySize(r)) return false;
      if(!(r[i].low<r[i-k].low && r[i].low<r[i+k].low)) return false; }
   return true;
}

double VolPercentile(const long &vol[], int i, int win){
   int a=i+win-1; if(a>=ArraySize(vol)) a=ArraySize(vol)-1; int b=i; int n=a-b+1; if(n<=1) return 0.0;
   int cnt=0; for(int k=b;k<=a;k++) if(vol[k]<=vol[i]) cnt++; return (double)cnt/(double)n;
}
double VolZ(const long &vol[], int i, int win){
   int a=i+win-1; if(a>=ArraySize(vol)) a=ArraySize(vol)-1; int b=i; int n=a-b+1; if(n<5) return 0.0;
   double s=0,s2=0; for(int k=b;k<=a;k++){ double v=(double)vol[k]; s+=v; s2+=v*v; }
   double m=s/n; double var=(s2/n) - m*m; if(var<1e-12) var=1e-12; return (vol[i]-m)/MathSqrt(var);
}
double CLV(double close,double high,double low){ double rng=high-low; if(rng<1e-8) rng=1e-8; return (close - low)/rng; }

//===================== Bias ========================================
bool GetMTFBias(bool &bullBias, bool &bearBias){
   bullBias=false; bearBias=false; if(!Use_MTF_Bias) return true;
   if(g_bias_ema==INVALID_HANDLE) return true;
   int need = Bias_Slope_Bars + 2; double ema[]; SetSeriesDouble(ema);
   if(CopyBuffer(g_bias_ema,0,0,need,ema) < need) return true;
   double s=0; for(int i=0;i<Bias_Slope_Bars;i++) s += (ema[i]-ema[i+1]); if(s>0) bullBias=true; if(s<0) bearBias=true;
   double px = SymbolInfoDouble(_Symbol, SYMBOL_BID); if(px>ema[0]) bullBias=true; if(px<ema[0]) bearBias=true; return true;
}

//===================== Sequencer & LPS =============================
struct SeqHit{ bool sos; bool sow; int break_index; int test_index; double level; long break_vol; };

bool DetectSequencer(const MqlRates &r[], const long &vol[], int i, int winRange, SeqHit &hit)
{
   hit.sos=false; hit.sow=false; hit.break_index=-1; hit.test_index=-1; hit.level=0.0; hit.break_vol=0;
   if(i+winRange>=ArraySize(r)) return false;

   double rHigh = r[i+winRange].high, rLow = r[i+winRange].low;
   for(int k=i+winRange; k>=i+1; --k){ if(r[k].high>rHigh) rHigh=r[k].high; if(r[k].low<rLow) rLow=r[k].low; }

   double body = MathAbs(r[i].close - r[i].open); double rng = r[i].high - r[i].low; if(rng<1e-8) rng=1e-8;
   double bodyFrac = body / rng; double vRank = VolPercentile(vol,i,VolRank_Window);

   bool upBreak = (r[i].close>rHigh) && (bodyFrac>=Seq_BodyFrac_Min) && (vRank>=Seq_VolRank_Min);
   bool dnBreak = (r[i].close<rLow)  && (bodyFrac>=Seq_BodyFrac_Min) && (vRank>=Seq_VolRank_Min);

   if(!(upBreak||dnBreak)) return false;

   int maxAhead = i-Seq_Lookahead_Bars; if(maxAhead<0) maxAhead=0;
   for(int t=i-1; t>=maxAhead; --t){
      double testVRank = VolPercentile(vol,t,VolRank_Window);
      bool lowVol = testVRank <= Seq_Test_VolPct_Max;
      if(upBreak){
         if(lowVol && r[t].low <= rHigh && r[t].close >= rHigh){ hit.sos=true; hit.break_index=i; hit.test_index=t; hit.level=rHigh; hit.break_vol=vol[i]; return true; }
      } else if(dnBreak){
         if(lowVol && r[t].high >= rLow && r[t].close <= rLow){ hit.sow=true; hit.break_index=i; hit.test_index=t; hit.level=rLow; hit.break_vol=vol[i]; return true; }
      }
   }
   return false;
}

bool DetectLPS(const MqlRates &r[], const long &vol[], const SeqHit &hit, int lastClosed, int &idxLPS, bool &isBull)
{
   idxLPS=-1; isBull=false; if(!LPS_Enable) return false;
   if(hit.sos){
      int minIdx = hit.test_index - LPS_MaxBars; if(minIdx<0) minIdx=0;
      for(int t=hit.test_index-1; t>=minIdx; --t){
         double depth = hit.level - r[t].low;
         double atrspan = r[lastClosed].high - r[lastClosed].low; if(atrspan<1e-8) atrspan=1e-8;
         if(depth >= 0 && depth <= LPS_TestDepth_ATR*atrspan){
            if((double)vol[t] <= (double)hit.break_vol * LPS_VolPct_Max){
               if(r[t].close >= hit.level){ idxLPS=t; isBull=true; return true; }
            }
         }
      }
   } else if(hit.sow){
      int minIdx2 = hit.test_index - LPS_MaxBars; if(minIdx2<0) minIdx2=0;
      for(int u=hit.test_index-1; u>=minIdx2; --u){
         double depth2 = r[u].high - hit.level;
         double atrspan2 = r[lastClosed].high - r[lastClosed].low; if(atrspan2<1e-8) atrspan2=1e-8;
         if(depth2 >= 0 && depth2 <= LPS_TestDepth_ATR*atrspan2){
            if((double)vol[u] <= (double)hit.break_vol * LPS_VolPct_Max){
               if(r[u].close <= hit.level){ idxLPS=u; isBull=false; return true; }
            }
         }
      }
   }
   return false;
}

//===================== CADS ========================================
struct CADSParts{ double structure; double evr; double clv; double mtf; double total; };

void RollingSR(const MqlRates &r[], int i, int lb, int win, double &S, double &R)
{
   S = r[i].low; R = r[i].high;
   int stop = i+win; if(stop>=ArraySize(r)) stop=ArraySize(r)-1;
   for(int k=i+1; k<=stop && k<ArraySize(r)-lb; ++k){
      if(IsSwingLow(r,k,lb))  if(r[k].low<S) S = r[k].low;
      if(IsSwingHigh(r,k,lb)) if(r[k].high>R) R = r[k].high;
   }
   for(int m=stop; m>=i+1; --m){ if(m>=ArraySize(r)) break; if(r[m].high>R) R=r[m].high; if(r[m].low<S) S=r[m].low; }
}

CADSParts ComputeCADS(const MqlRates &r[], const long &vol[], const double &atr[], int i)
{
   CADSParts p; p.structure=0; p.evr=0; p.clv=0; p.mtf=0; p.total=0;
   if(i+Range_Window>=ArraySize(r)) return p;

   double R,S; RollingSR(r,i,Swing_Lookback,Range_Window,S,R);
   double pos = CLV(r[i].close, R, S);

   double clv_acc = 1.0 - pos;
   double clv_dist= pos;

   double struct_acc=0, struct_dist=0;
   bool swH = IsSwingHigh(r,i,Swing_Lookback);
   bool swL = IsSwingLow(r,i,Swing_Lookback);
   if(swL && r[i].close>r[i+1].close) struct_acc+=0.7;
   if(swH && r[i].close<r[i+1].close) struct_dist+=0.7;
   if(r[i].close > r[i+1].high) struct_acc+=0.3;
   if(r[i].close < r[i+1].low)  struct_dist+=0.3;

   double z = VolZ(vol,i,VolRank_Window);
   double atrspan = atr[i]; if(atrspan<1e-8) atrspan=1e-8;
   double spreadATR = (r[i].high - r[i].low) / atrspan;

   double evr_comp = 0.0;
   if(z>=EVR_Effort_Z_Min){
      if(MathAbs(r[i].close - S) <= EVR_Absorb_Prox_ATR*atr[i] && spreadATR < EVR_Result_MinATR) evr_comp += 1.0;
      if(MathAbs(r[i].close - R) <= EVR_Absorb_Prox_ATR*atr[i] && spreadATR < EVR_Result_MinATR) evr_comp -= 1.0;
   }
   if(spreadATR >= (EVR_Result_MinATR*1.25)){
      if(r[i].close>r[i].open && pos<0.5) evr_comp += 0.6;
      if(r[i].close<r[i].open && pos>0.5) evr_comp -= 0.6;
   }

   bool bull=false, bear=false; GetMTFBias(bull,bear);
   double mtf_comp = 0.0; if(bull) mtf_comp+=1.0; if(bear) mtf_comp-=1.0;

   double structure = struct_acc - struct_dist;
   double clv_comp  = clv_acc - clv_dist;

   double score = 0.0;
   score += W_Structure * structure;
   score += W_EVR       * evr_comp;
   score += W_CLV       * clv_comp;
   score += W_MTF       * mtf_comp;

   double total = 50.0 + 50.0 * score; if(total<0.0) total=0.0; if(total>100.0) total=100.0;
   p.structure = 50.0 + 50.0*structure;
   p.evr       = 50.0 + 50.0*evr_comp;
   p.clv       = 50.0 + 50.0*clv_comp;
   p.mtf       = 50.0 + 50.0*mtf_comp;
   p.total     = total;
   return p;
}

//===================== Alerts & Persistence ========================
void FireAlert(const string msg, datetime &lastT){
   if(!Alerts_Enable) return;
   datetime now = TimeCurrent();
   if(now - lastT < Alert_Cooldown_Sec) return;
   lastT = now;
   if(Alerts_Popup) Alert(msg);
   if(Alerts_Sound) PlaySound(Alerts_Sound_File);
   if(Alerts_Push)  SendNotification(msg);
}
void AppendCSV(const string tag, datetime t, double price, double score){
   if(!Persist_ToCSV) return;
   int fh = FileOpen("CADS_signals.csv", FILE_READ|FILE_WRITE|FILE_CSV|FILE_ANSI);
   if(fh==INVALID_HANDLE){
      // try create file (write mode)
      fh = FileOpen("CADS_signals.csv", FILE_WRITE|FILE_CSV|FILE_ANSI);
      if(fh==INVALID_HANDLE) return; // still failed => give up
   }
   FileSeek(fh, 0, SEEK_END);
   FileWrite(fh, TimeToString(t, TIME_DATE|TIME_MINUTES), _Symbol, TFToStr(_Period), tag,
             DoubleToString(price, _Digits), (int)score);
   FileClose(fh);
}
void DrawMarker(datetime t,double price,int type){
   if(!Draw_Signal_Markers) return;
   string name = PFX + "MK_" + IntegerToString((int)t) + "_" + IntegerToString(type);
   if(ObjectFind(0,name)!=-1) return;
   int arrowCode = (type==1? 241 : (type==2? 242 : (type==3? 251 : (type==4? 217 : 218))));
   color c = (type==1? Color_SOS : (type==2? Color_SOW : (type==3? Color_ZoneChange : (type==4? Color_LPS : Color_LPSY))));
   ObjectCreate(0,name,OBJ_ARROW,0, t, price);
   ObjectSetInteger(0,name,OBJPROP_ARROWCODE,arrowCode);
   ObjectSetInteger(0,name,OBJPROP_COLOR,c);
   ObjectSetInteger(0,name,OBJPROP_WIDTH,1);
}

//===================== Liquidity/Efficiency proxies (no FVG) ======
int CountOppStacks_NoFVG(bool wantLong, const MqlRates &r[], const double &atr[], int last, int scanBars){
   // proxy: count number of recent "wide" bars against direction that closed back inside range -> potential digestion
   int cnt=0; int stop = MathMax(1, last+scanBars); if(stop>=ArraySize(r)) stop=ArraySize(r)-1;
   for(int i=last; i<=stop; ++i){
      double tr = r[i].high - r[i].low; double a = atr[i]; if(a<1e-8) a=1e-8;
      bool wide = (tr >= Eff_Wide_TR_to_ATR * a);
      bool against = (wantLong? (r[i].close<r[i].open) : (r[i].close>r[i].open));
      bool inside = ( (MathAbs(r[i].close - r[i].open) <= Eff_CloseInsideFrac * tr) );
      if(wide && against && inside) cnt++;
   }
   return cnt;
}

//===================== Direction HUD ===============================
enum DirState { DIR_SHORT = -1, DIR_HOLD = 0, DIR_LONG = 1 };

void DrawTinyChip(const string name,const string label,int x,int y,color bg,int fs){
   string box = name+"B";
   if(ObjectFind(0,box)==-1) ObjectCreate(0,box,OBJ_RECTANGLE_LABEL,0,0,0);
   ApplyCorner(box);
   ObjectSetInteger(0,box,OBJPROP_XDISTANCE,x);
   ObjectSetInteger(0,box,OBJPROP_YDISTANCE,y);
   int w = 6 + (int)StringLen(label)*(fs-1); if(w<20) w=20;
   int h = fs + 4;
   ObjectSetInteger(0,box,OBJPROP_XSIZE,w);
   ObjectSetInteger(0,box,OBJPROP_YSIZE,h);
   ObjectSetInteger(0,box,OBJPROP_COLOR,bg);
   ObjectSetInteger(0,box,OBJPROP_BGCOLOR,bg);
   ObjectSetInteger(0,box,OBJPROP_BACK,true);
   ObjectSetInteger(0,box,OBJPROP_SELECTABLE,false);

   string txt = name+"T";
   if(ObjectFind(0,txt)==-1) ObjectCreate(0,txt,OBJ_LABEL,0,0,0);
   ApplyCorner(txt);
   ObjectSetInteger(0,txt,OBJPROP_XDISTANCE,x+3);
   ObjectSetInteger(0,txt,OBJPROP_YDISTANCE,y+1);
   ObjectSetInteger(0,txt,OBJPROP_FONTSIZE,fs);
   ObjectSetInteger(0,txt,OBJPROP_COLOR,EG_Text);
   ObjectSetInteger(0,txt,OBJPROP_SELECTABLE,false);
   ObjectSetString(0,txt,OBJPROP_TEXT,label);
}

void DrawDirectionHUD(DirState dir, const string reason){
   if(!EG_Enable) return;
   int x = HUD_X, y = HUD_Y + EG_OffsetY;

   string ltext, stext;
   color  lcol, scol;

   if(dir==DIR_LONG){
      ltext = "L: LONG";
      stext = "S: HOLD";
      lcol  = EG_Green;
      scol  = EG_Hold_Color;
   }else if(dir==DIR_SHORT){
      ltext = "L: HOLD";
      stext = "S: SHORT";
      lcol  = EG_Hold_Color;
      scol  = EG_Red;
   }else{
      ltext = "L: HOLD";
      stext = "S: HOLD";
      lcol  = EG_Hold_Color;
      scol  = EG_Hold_Color;
   }

   DrawTinyChip(PFX+"EG_L", ltext, x, y, lcol, EG_FontSize);

   int l_w = 6 + (int)StringLen(ltext)*(EG_FontSize-1); if(l_w<20) l_w=20;
   int x2  = x + l_w + EG_Spacing_X;

   DrawTinyChip(PFX+"EG_S", stext, x2, y, scol, EG_FontSize);

   string rn = PFX+"EG_R";
   if(ObjectFind(0,rn)==-1) ObjectCreate(0,rn,OBJ_LABEL,0,0,0);
   ApplyCorner(rn);
   ObjectSetInteger(0,rn,OBJPROP_XDISTANCE,x);
   ObjectSetInteger(0,rn,OBJPROP_YDISTANCE,y + EG_FontSize + 6);
   ObjectSetInteger(0,rn,OBJPROP_FONTSIZE,EG_FontSize-1);
   ObjectSetInteger(0,rn,OBJPROP_COLOR,HUD_Accent);
   ObjectSetInteger(0,rn,OBJPROP_SELECTABLE,false);
   ObjectSetString(0,rn,OBJPROP_TEXT,"Reason: "+reason);
}

//===================== HUD ========================================
void DrawHUD(const CADSParts &p, double evrRow1, double evrRow2, const string extra)
{
   int x=HUD_X, y=HUD_Y;
   string ttl = "CADS Wyckoff | " + _Symbol + " " + TFToStr(_Period);
   EnsureLabel(OBJ_TITLE, ttl, x, y-14, 9, HUD_Label, true);

   string info =
      "Now "  + TimeToString(g_live_time, TIME_MINUTES) +
      "  O:"  + DoubleToString(g_live_open,  2) +
      " H:"   + DoubleToString(g_live_high,  2) +
      " L:"   + DoubleToString(g_live_low,   2) +
      " C:"   + DoubleToString(g_live_close, 2) +
      "  Vol:"+ DoubleToString((double)g_live_vol, 0) +
      "  ATR:"+ DoubleToString(g_live_atr,   2) + extra;
   EnsureLabel(OBJ_INFO, info, x, y, 8, HUD_Text, true);

   color dial = CADS_Color_Neutral;
   if(p.total>=CADS_Accum_Threshold) dial = CADS_Color_Accum;
   else if(p.total<=CADS_Dist_Threshold) dial = CADS_Color_Dist;
   EnsureLabel(OBJ_SCORE, "CADS: " + IntegerToString((int)MathRound(p.total)), x, y+14, 12, dial, true);

   int hbX = x + 40;
   int hbY = y + 32;
   EnsureLabel(OBJ_EVR1LBL, "1) Effort", x, hbY-2, 8, HUD_Label, true);
   EnsureLabel(OBJ_EVR2LBL, "2) Result", x, hbY + Heat_Height + 4 - 2, 8, HUD_Label, true);

   EnsureHeatbar(OBJ_HEAT1, hbX, hbY, Heat_Width, Heat_Height, Heat_Segments);
   EnsureHeatbar(OBJ_HEAT2, hbX, hbY + Heat_Height + 4, Heat_Width, Heat_Height, Heat_Segments);
   ColorHeatbar(OBJ_HEAT1, Heat_Segments, (evrRow1<0?0.0:(evrRow1>1.0?1.0:evrRow1)));
   ColorHeatbar(OBJ_HEAT2, Heat_Segments, (evrRow2<0?0.0:(evrRow2>1.0?1.0:evrRow2)));
}

//===================== Lifecycle ==================================
int OnInit(){
   g_atr      = iATR(_Symbol, _Period, ATR_Period);
   g_bias_ema = iMA(_Symbol, BiasTF, Bias_EMA_Period, 0, MODE_EMA, PRICE_CLOSE);
   g_htf_ema  = iMA(_Symbol, HTF_for_MacroIneff, Bias_EMA_Period, 0, MODE_EMA, PRICE_CLOSE);
   return(INIT_SUCCEEDED);
}
void OnDeinit(const int reason){
   // delete our objects
   int total = ObjectsTotal(0); // use standard form
   for(int i=total-1;i>=0;--i){
      string nm = ObjectName(0,i); // correct signature
      if(StringFind(nm,PFX)==0) ObjectDelete(0,nm);
   }
   // release indicator handles
   if(g_atr != INVALID_HANDLE){ IndicatorRelease(g_atr); g_atr = INVALID_HANDLE; }
   if(g_bias_ema != INVALID_HANDLE){ IndicatorRelease(g_bias_ema); g_bias_ema = INVALID_HANDLE; }
   if(g_htf_ema != INVALID_HANDLE){ IndicatorRelease(g_htf_ema); g_htf_ema = INVALID_HANDLE; }
}

// Compute simple AVWAP (tick volume proxy)
double ComputeAVWAP(const MqlRates &r[], const long &vol[], int anchorIndexSeries, int lastClosed){
   if(anchorIndexSeries<0) return 0.0;
   double pv=0.0, vv=0.0;
   int start = anchorIndexSeries;
   for(int ii=start; ii>=lastClosed; ii--){
      pv += ((r[ii].open+r[ii].close)*0.5) * (double)vol[ii];
      vv += (double)vol[ii];
      if(start - ii > CPU_MaxBars_Backfill) break;
   }
   if(vv<=0) return 0.0;
   return pv/vv;
}

// Full-array signature (robust)
int OnCalculate(const int rates_total,
                const int prev_calculated,
                const datetime &time[],
                const double   &open[],
                const double   &high[],
                const double   &low[],
                const double   &close[],
                const long     &tick_volume[],
                const long     &volume[],
                const int      &spread[])
{
   int need = MathMax(ATR_Period, MathMax(VolRank_Window, Range_Window)) + 10;
   if(rates_total < need) return 0;

   MqlRates r[];
   int n=CopyRates(_Symbol,_Period,0,rates_total,r);
   if(n<=0) return 0;
   SetSeriesRates(r);

   long vol[];
   ArrayResize(vol,n);
   SetSeriesLong(vol);
   for(int i=0;i<n;i++)
      vol[i] = (volume[i] > 0 ? volume[i] : r[i].tick_volume);

   double atrbuf[];
   SetSeriesDouble(atrbuf);
   if(g_atr==INVALID_HANDLE) g_atr = iATR(_Symbol, _Period, ATR_Period);
   if(CopyBuffer(g_atr,0,0,n,atrbuf) <= 1) return 0;

   int form = 0, last = 1;
   if(n<2) return 0;
   const int eg_i = 1; // closed bar for direction decisions

   g_live_time = r[form].time;
   g_live_open = r[form].open;
   g_live_high = r[form].high;
   g_live_low  = r[form].low;
   g_live_close= r[form].close;
   g_live_vol  = vol[form];
   g_live_atr  = atrbuf[last];

   // EVR rows
   double vRank   = VolPercentile(vol, form, VolRank_Window);
   double evrRow1 = vRank;
   double atrspan = atrbuf[last]; if(atrspan<1e-8) atrspan=1e-8;
   double evrRow2 = (r[form].high - r[form].low) / atrspan;
   if(evrRow2>2.0) evrRow2=2.0;
   evrRow2 *= 0.5;

   // CADS
   CADSParts parts = ComputeCADS(r,vol,atrbuf,last);

   // Sequencer & markers
   SeqHit hit;
   bool got = DetectSequencer(r,vol,eg_i, Swing_Lookback*4, hit);
   bool gotSOS=false, gotSOW=false, gotLPS=false, gotLPSY=false;
   if(got){
      string tag = (hit.sos? "SOS>Test" : (hit.sow? "SOW>Test" : ""));
      if(hit.sos){
         gotSOS=true;
         FireAlert("[CADS] "+tag+" "+_Symbol+" "+TFToStr(_Period)+" @ "+DoubleToString(r[hit.test_index].close,_Digits), g_lastAlert_SOS);
         DrawMarker(r[hit.test_index].time, r[hit.test_index].close, 1);
         AppendCSV(tag, r[hit.test_index].time, r[hit.test_index].close, parts.total);
      }else if(hit.sow){
         gotSOW=true;
         FireAlert("[CADS] "+tag+" "+_Symbol+" "+TFToStr(_Period)+" @ "+DoubleToString(r[hit.test_index].close,_Digits), g_lastAlert_SOW);
         DrawMarker(r[hit.test_index].time, r[hit.test_index].close, 2);
         AppendCSV(tag, r[hit.test_index].time, r[hit.test_index].close, parts.total);
      }
      int idxLPS; bool bull;
      if(DetectLPS(r,vol,hit,last,idxLPS,bull)){
         if(bull){
            gotLPS=true;
            DrawMarker(r[idxLPS].time, r[idxLPS].close, 4);
            AppendCSV("LPS",  r[idxLPS].time, r[idxLPS].close, parts.total);
         }else{
            gotLPSY=true;
            DrawMarker(r[idxLPS].time, r[idxLPS].close, 5);
            AppendCSV("LPSY", r[idxLPS].time, r[idxLPS].close, parts.total);
         }
      }
   }

   // Zone transitions
   static int prev_zone = -1;
   int zone = 1;
   if(parts.total>=CADS_Accum_Threshold) zone=2;
   else if(parts.total<=CADS_Dist_Threshold) zone=0;
   if(zone != prev_zone){
      string zname = (zone==2? "ACCUM" : zone==0? "DIST" : "NEUTRAL");
      string zmsg  = "[CADS] Zone: " + zname + " score=" +
                     IntegerToString((int)MathRound(parts.total)) + " " +
                     _Symbol + " " + TFToStr(_Period);
      FireAlert(zmsg, g_lastAlert_CADS);
      DrawMarker(r[last].time, r[last].close, 3);
      AppendCSV("Zone:"+zname, r[last].time, r[last].close, parts.total);
      prev_zone = zone;
   }

   // AVWAP (simple anchors)
   double avwap=0.0;
   if(AVWAP_Enable){
      if(g_anchor_index_SC<0 && AVWAP_From_SC){
         int stop1 = MathMin(n-2, CPU_MaxBars_Backfill);
         for(int ii=stop1; ii>=1; ii-=CPU_Scan_Step){
            double z2 = VolZ(vol,ii,VolRank_Window);
            if(z2>=EVR_Effort_Z_Min*2.0 && r[ii].close<r[ii].open){
               g_anchor_index_SC=ii; break;
            }
         }
      }
      if(g_anchor_index_BC<0 && AVWAP_From_BC){
         int stop2 = MathMin(n-2, CPU_MaxBars_Backfill);
         for(int jj=stop2; jj>=1; jj-=CPU_Scan_Step){
            double z3 = VolZ(vol,jj,VolRank_Window);
            if(z3>=EVR_Effort_Z_Min*2.0 && r[jj].close>r[jj].open){
               g_anchor_index_BC=jj; break;
            }
         }
      }
      int anchor = g_anchor_index_SC;
      if(anchor<0) anchor=g_anchor_index_BC;
      avwap = ComputeAVWAP(r,vol,anchor,last);
      if(avwap>0.0){
         string nm = OBJ_AVWAP;
         if(ObjectFind(0,nm)==-1) ObjectCreate(0,nm,OBJ_HLINE,0,0,0);
         ObjectSetDouble(0,nm,OBJPROP_PRICE, avwap);
         ObjectSetInteger(0,nm,OBJPROP_COLOR, clrDarkSlateBlue);
         ObjectSetInteger(0,nm,OBJPROP_STYLE, STYLE_DOT);
      }
   }

   // ------------ Simple Direction Engine (Long / Hold / Short) ------------
   // Volume context
   double egVolPct = VolPercentile(vol, eg_i, VolRank_Window);

   // Structural / CADS context
   bool cadsBull = (parts.total >= CADS_Accum_Threshold);
   bool cadsBear = (parts.total <= CADS_Dist_Threshold);

   // HTF bias
   bool bullBias=false, bearBias=false;
   GetMTFBias(bullBias,bearBias);

   bool longCond  = cadsBull && bullBias && (egVolPct >= EG_MinVolPercentile);
   bool shortCond = cadsBear && bearBias && (egVolPct >= EG_MinVolPercentile);

   DirState dir = DIR_HOLD;
   if(longCond && !shortCond)
      dir = DIR_LONG;
   else if(shortCond && !longCond)
      dir = DIR_SHORT;
   else if(longCond && shortCond)
      dir = (parts.total >= 50.0 ? DIR_LONG : DIR_SHORT);
   else
      dir = DIR_HOLD;

   // Alert on direction change
   static int prevDir = 999;
   if(prevDir==999) prevDir = (int)DIR_HOLD;
   if((int)dir != prevDir){
      string dtext = (dir==DIR_LONG ? "LONG" : (dir==DIR_SHORT ? "SHORT" : "HOLD"));
      string msg   = "[CADS] Direction: " + dtext + " " + _Symbol + " " + TFToStr(_Period) +
                     " score=" + IntegerToString((int)MathRound(parts.total));
      FireAlert(msg, g_lastAlert_EG);
      prevDir = (int)dir;
   }

   string reason = "neutral";
   if(dir==DIR_LONG)
      reason = "HTF bull + CADS long";
   else if(dir==DIR_SHORT)
      reason = "HTF bear + CADS short";
   else{
      if(egVolPct < EG_MinVolPercentile) reason = "low volume / chop";
      else if(!cadsBull && !cadsBear)    reason = "CADS neutral zone";
      else                               reason = "mixed HTF / CADS";
   }

   DrawDirectionHUD(dir, reason);

   // HUD note
   string extra = StringFormat(" | vol%%=%.2f", egVolPct);
   DrawHUD(parts, evrRow1, evrRow2, extra);
   return(rates_total);
}
