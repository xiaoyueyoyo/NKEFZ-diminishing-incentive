cd "/Users/xiaoyue/PostPHD/Rcode/LGEE"
import import delimited "panelnew.csv", clear 
merge m:m id year using "paneldata.dta", keepusing(province id year 预算收入 预算支出 eft nkefz EFT) nogenerate
variable 预算收入预算支出eft not found

**计算人均时统一使用常住人口，而非户籍人口)

xtset id year

**生成相关变量
gen EFT = log(eft/年末常住)
replace EFT = 0 if nkefz == 0
gen GDP = log(地区生产*10e4/年末常住)
gen LGEE = log(地方财政环境*10e4/年末常住)
gen URB = 100*城镇人口/年末常住
gen EDU = log(普通高等)  //在校生人数)
gen OPEN = log(0.1*经营单位/年末常住)  //由占gdp比重改为人均美元了。
gen FCR = 森林
gen SO2 = log(二氧化硫*10e6/年末常住)  //由人均kg改为人均g了
gen IND = 100*第二产业/地区生产  
replace SUF = 100*预算收入/预算支出 

**填补缺失值
missings report
autofill,forward groupby(id)
autofill,backwoard groupby(id)
save filledpanel.dta
use filledpanel.dta

**目前没有缺失值，除了原有的eft(考虑了非NKEFZ的少量年份的EFT数据

save LGEEpanel09-22.dta,replace
use LGEEpanel09-22.dta,clear

drop if nkefz == 0
save NKEFZpanel.dta,replace


**保存结果

cd "/Users/xiaoyue/PostPHD/Rcode/LGEE/results"
use NKEFZpanel.dta,clear
xtset id year

//表一：描述性统计分析----------------------------------------

gen OPEN_dollar = OPEN
replace OPEN = 6.5 *OPEN_dollar
sum LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 
outreg2 using LGEE_描述性统计.docx, replace sum(log) keep(LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2) title(Descriptive statistics) 

//相关性分析--------------------------------
pwcorr LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 
//多重共线性检验----------------------------
reg LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 
estat vif
*没有vif大于10，不存在多重共线性问题


//OLS RE FE基础选择-------------------------
xtset id year
xi:xtreg  LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 ,re
est store re
*Wald检验通过。RE优于OLS
xi:xtreg LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2,fe
est store fe
*F检验通过。FE优于OLS
hausman fe re
*基础Hausman检验，P值小于0.1，可以拒绝原假设，FE优于RE


//时间固定效应
xi:xtreg LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 i.year,fe
test _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022 
*年份虚拟变量联合显著性通过，选择时间固定效应


//异方差，自相关，截面相关检验-----------------
xi:xtreg LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 i.year,fe
xttest3
*异方差检验结果显著，存在异方差
xtserial LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2
*自相关结果显著，存在自相关
//截面相关
xi:xtreg LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 i.year ,fe
xtcsd,pes
xtcsd,fri
xtcsd,fre
*存在截面相关

//三大问题纠正----------------------------------
//方法一：robust 聚类稳健误，修正前两个
xi:xtreg LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 i.year ,fe r
//方法二：短面板。慎用xtscc
xi:xtscc LGEE EFT GDP SUF URB EDU OPEN IND FCR SO2 i.year ,fe 
*目前使用xtreg,r；xtscc结果仍旧不显著
//方法三：换模型，加入EFT二次项
gen EFT2 = EFT^2
save NKEFZpanel.dta,replace

xi:xtreg  LGEE EFT i.year,fe r
quietly outreg2 using xtreg2f.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT) addtext(Province FE, YES,Year FE, YES)
xi:xtreg  LGEE EFT EFT2 i.year,fe r
quietly outreg2 using xtreg2f.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2) addtext(Province FE, YES,Year FE, YES)
xi:xtreg  LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year,fe r
quietly outreg2 using xtreg2f.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
xi:xtreg  LGEE EFT EFT2 GDP SUF URB EDU OPEN IND FCR SO2 i.year ,fe r
quietly outreg2 using xtreg2f.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND FCR SO2) addtext(Province FE, YES,Year FE, YES)
*倒U，均在10%水平上显，组间R方0.8


xi:xtreg LGEE EFT i.year, fe r
xthreg 

//表二：基准模型-----------------------------------------------
*一期滞后项和二期滞后项做工具变量
quietly xtivreg2 LGEE (EFT=l.EFT l2.EFT),fe r gmm2s first
outreg2 using 表一.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2 LGEE (EFT=l.EFT l2.EFT) GDP SUF URB EDU OPEN IND,fe r gmm2s first
outreg2 using 表一.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, NO)
quietly xi: xtivreg2 LGEE (EFT=l.EFT l2.EFT) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first
outreg2 using 表一.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)

*一期滞后项做工具变量
quietly xtivreg2 LGEE (EFT=l.EFT),fe r gmm2s first
outreg2 using 表一.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2 LGEE (EFT=l.EFT) GDP SUF URB EDU OPEN IND,fe r gmm2s first
outreg2 using 表一.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, NO)
quietly xi: xtivreg2 LGEE (EFT=l.EFT) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first
outreg2 using 表一.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)



  
//表三：二次项模型-----------------------------------------------

*一期二期滞后项做工具变量
quietly  xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) ,fe r gmm2s first
outreg2 using 表三.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT EFT2) addtext(Province FE, YES,Year FE, NO)
quietly  xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF EDU ,fe r gmm2s first
outreg2 using 表三.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF) addtext(Province FE, YES,Year FE, NO)
quietly xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND,fe r gmm2s first
outreg2 using 表三.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, NO)
quietly xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year ,fe r gmm2s first
outreg2 using 表三.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)


//qfit 显示都为正U型关系
twoway (scatter LGEE EFT) (qfit LGEE EFT, lcolor(red))
twoway (scatter LGEE l.EFT) (qfit LGEE l.EFT, lcolor(red))
twoway (scatter LGEE l2.EFT) (qfit LGEE l2.EFT, lcolor(red))


//U型检测
xi:xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first
xi:xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year,fd r gmm2s first
*由于二阶段会进行差分，常数项是不被汇报的，尝试使用fd。不显著
*xtivreg2 可以使用gmm2s和robust
xtivreg LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year,fe //倒U型仍然显著
utest EFT EFT2,quadratic //p=0.0731显著
*但是为汇报常数项，可以使用xtivreg,但无robust和gmm2s
global tp: dis %3.1f -_b[EFT]/(2*_b[EFT2])
dis "Turn point = " $tp
//Turn point = 2.8
global b0 = _b[_cons]
global b1 = _b[EFT]
global b2 = _b[EFT2]
sum EFT
global min:  dis %3.1f r(min)
global max:  dis %3.1f r(max)
#delimit
twoway (function y =-.079*x^2 + 0.436*x -4.376,range(0.299 6.924)) ///
       (function y =-.079*x^2 + 0.436*x -4.376, range(6.924 9) lp(dash)), ///
       ytitle("LGEE") xtitle("EFT") ///
       xline(0.299,lc(red) lw(*1.5)) ///
       xline(2.8,  lp(dash) lc(green)) ///
       xline(6.924,lc(red) lw(*1.5)) ///
       xlabel(0.299 "Min (0.299)" 0(3)9 2.8 "Turnpoint(2.8)" 6.924 "Max (6.564)", angle(50)) ///
       legend(off)
graph export "fig_Ushape.png", replace  //保存图形
//LGEE在范围（6.87,9.75）内
twoway (function y =-.079*x^2 + 0.436*x +7.376,range(0.299 6.924)) ///
       (function y =-.079*x^2 + 0.436*x +7.376, range(6.924 9) lp(dash)), ///
       ytitle("LGEE") xtitle("EFT") ///
       xline(0.299,lc(red) lw(*1.5)) ///
       xline(2.8,  lp(dash) lc(green)) ///
       xline(6.924,lc(red) lw(*1.5)) ///
       xlabel(0.299 "Min (0.299)" 0(3)9 2.8 "Turnpoint(2.8)" 6.924 "Max (6.564)", angle(50)) ///
       legend(off)
graph export "IU2.png", replace  //保存图形

//FCR门槛U型图
twoway (function y =-.058*x^2 + 0.368*x +7.376,range(0.299 6.924)) ///
       (function y =-.058*x^2 + 0.368*x +7.376, range(6.924 9) lp(dash)), ///
       ytitle("LGEE") xtitle("EFT") ///
       xline(0.299,lc(red) lw(*1.5)) ///
       xline(3.17,  lp(dash) lc(green)) ///
       xline(6.924,lc(red) lw(*1.5)) ///
       xlabel(0.299 "Min (0.299)" 0(3)9 3.17 "Turnpoint(3.17)" 6.924 "Max (6.564)", angle(50)) ///
       legend(off)
graph export "IU3.png", replace  //保存图形





//robustness
*模型中加入三次项，考察是否为S曲线
gen EFT3 = EFT^3
xi:xtreg LGEE EFT EFT2 EFT3 GDP SUF URB EDU OPEN IND i.year,fe r
xi:xtivreg2 LGEE (EFT EFT2 EFT3=l2.EFT l.EFT l.EFT2 l2.EFT2 l.EFT3 l2.EFT3) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first
//加入EFT3的滞后项的话是显著的，不加则三次项不显著。说明不存在S型曲线

*断点回归
gen tp = 2.8
gen lowEFT = 0
gen highEFT = 0
replace lowEFT = EFT-2.8 if EFT<2.8
replace highEFT = EFT-2.8 if EFT>2.8
gen high = 0
replace high = 1 if EFT>2.8
xtreg LGEE lowEFT highEFT high,fe r   //lowEFT highEFT 均显著。其他控制变量省略。但都为正
xtreg LGEE lowEFT highEFT high GDP SUF URB EDU OPEN IND,fe r  //加入控制变量后，一正一负，low不显著
xi: xtreg LGEE lowEFT highEFT high GDP SUF URB EDU OPEN IND i.year,fe r  //加入时间效应后，一正一负，low不显著
xi: xtivreg2 LGEE (lowEFT highEFT=l.lowEFT l.highEFT) high GDP SUF URB EDU OPEN IND i.year,fe gmm2s first r



xthreg LGEE GDP SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT) qx(EFT) thnum(1) trim(0.05) grid(400) bs(300) robust 

xi:xtivreg LGEE (EFT=l.EFT) GDP SUF URB EDU OPEN IND i.year,fe
xi:xtivreg LGEE (EFT EFT2 = l.EFT l2.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year,fe //用上两个滞后项做工具变量就好一些了。






 //表四：区域异质性-----------------------------------------------
 
quietly xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r gmm2s first  //东部地区 
outreg2 using 表四.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r gmm2s first  //中部地区 
outreg2 using 表四.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r gmm2s first  //西部地区 U型
outreg2 using 表四.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)


 quietly xi: xtreg LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r  //东部地区 存在U型
outreg2 using 表五.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
 quietly xi: xtreg LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r  //中部地区  不存在
outreg2 using 表五.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
 quietly xi: xtreg LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r  //西部地区  不存在)
outreg2 using 表五.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)



gen region = ""

replace region = "E" if inlist(id, 1, 2,3,6,7,8,9,10,11,13,15,19,21)
replace region = "W" if inlist(id, 5,20)| inrange(id,22,31)
replace region = "M" if inlist(id,4,12,14,16,17,18)
save NKEFZpanel.dta,replace


//无U型关系的门槛模型--------------------------


**森林覆盖率
xthreg LGEE GDP SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT) qx(FCR) thnum(3) trim(0.05 0.05 0.05) grid(400) bs(300 300 300) robust //三门槛检验，单门槛通过检验
xthreg LGEE GDP SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT) qx(FCR) thnum(1) trim(0.05) grid(400) bs(300) robust //三门槛检验，单门槛通过检验
_matplot e(LR), yline(7.35, lpattern(dash)) connect(direct) msize(small) mlabp(0) mlabs(zero) ytitle("LR Statistics") xtitle("Threshold") recast(line) name(FCR)
//画图效果还行


**二氧化硫
xthreg LGEE SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT) qx(SO2) thnum(3) trim(0.01 0.01 0.01) grid(400) bs(200 200 200) robust //三门槛，都不显著

xthreg LGEE SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT EFT2) qx(SO2) thnum(1) trim(0.01) grid(400) bs( 200) robust //双门槛，单门槛都不显著



//考虑U型关系的门槛模型-------------------
**森林覆盖率
xthreg LGEE GDP SUF URB EDU OPEN IND,rx(EFT EFT2) qx(FCR) thnum(3) trim(0.05 0.05 0.05) grid(400) bs(300 300 300) robust //考虑了U型关系的三门槛检验，单门槛通过

xthreg LGEE GDP SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT EFT2) qx(FCR) thnum(3) trim(0.05 0.05 0.05) grid(400) bs(300 300 300) robust //加入了时间固定效应，只有单门槛通过

xthreg LGEE GDP SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT EFT2) qx(FCR) thnum(2) trim(0.05 0.05) grid(400) bs(300 300) robust //加入了时间固定效应，只有单门槛通过

xthreg LGEE GDP SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT EFT2) qx(FCR) thnum(1) trim(0.05) grid(400) bs(300) robust //加入了时间固定效应，只有单门槛通过
_matplot e(LR), yline(7.35, lpattern(dash)) connect(direct) msize(small) mlabp(0) mlabs(zero) ytitle("LR Statistics") xtitle("Threshold") recast(line) name(Ufcr)
//效果还行。说明超过24的森林覆盖率后，才有比较明显的倒U型关系。


xthreg LGEE GDP SUF URB EDU OPEN IND,rx(EFT EFT2) qx(l.FCR) thnum(3) trim(0.05 0.05 0.05) grid(400) bs(300 300 300) robust //考虑了U型关系的三门槛检验，单门槛通过

xthreg LGEE GDP SUF URB EDU OPEN IND,rx(EFT EFT2) qx(l.FCR) thnum(2) trim(0.05 0.05) grid(400) bs(300 300) robust //考虑了滞后一期的双门槛


**二氧化硫
xthreg LGEE SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT EFT2) qx(SO2) thnum(2) trim(0.05 0.05) grid(400) bs(200 200) robust //考虑了U型关系的双门槛，都不显著


xthreg LGEE SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT EFT2) qx(l.SO2) thnum(2) trim(0.05 0.05) grid(400) bs(200 200) robust //滞后一期SO2的单门槛显著

xthreg LGEE SUF URB EDU OPEN IND _Iyear_2010 _Iyear_2011 _Iyear_2013 _Iyear_2014 _Iyear_2015 _Iyear_2016  _Iyear_2017 _Iyear_2018 _Iyear_2019 _Iyear_2020 _Iyear_2021 _Iyear_2022,rx(EFT EFT2) qx(l.SO2) thnum(1) trim(0.05) grid(400) bs(200) robust //滞后一期SO2的单门槛显著



**二氧化硫双门槛画图
_matplot e(LR21), columns(1 2) yline(7.35, lpattern(dash)) connect(direct) msize(small) mlabp(0) mlabs(zero) ytitle("LR Statistics") xtitle("First Threshold") recast(line) name(LR21) nodraw
_matplot e(LR22), columns(1 2) yline(7.35, lpattern(dash)) connect(direct) msize(small) mlabp(0) mlabs(zero) ytitle("LR Statistics") xtitle("Second Threshold") recast(line) name(LR22) nodraw
graph combine LR21 LR22, cols(1) //画图效果一般，比较多杂音


**Robustness check of the forest threshold 
//1.heterogeneity 
xi: xtreg LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year if FCR<14.10,fe r  //IU FCR<14.10
xi: xtreg LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year if FCR>14.10,fe r   //IU FCR>14.10
//2.interaction 不适合做robust 是另一个东西
gen INT = FCR*EFT
gen INT2 = FCR*EFT2
xi: xtivreg2 LGEE (EFT = l.EFT l2.EFT) INT FCR GDP SUF URB EDU OPEN IND i.year,fe r  //EFT负，INT正
xi: xtivreg2 LGEE (EFT EFT2= l.EFT l2.EFT l.EFT2 l2.EFT2) INT INT2 FCR GDP SUF URB EDU OPEN IND i.year,fe r  


//挤入模型
gen BPS = log(地方财政一般公共服务*10e4/年末常住)
replace PROD = log((地方财政农林水+地方财政科学技术)*10e4/年末常住)
replace LIVE = log((地方财政医疗+地方财政文化+地方财政社会保障+地方财政教育)*10e4/年末常住)
save NKEFZpanel.dta,replace

**LIVE支出
quietly xtivreg2 LIVE (EFT=l.EFT l2.EFT),fe r gmm2s first
outreg2 using LIVE.doc, replace tstat bdec(3) tdec(2) keep(LIVE EFT) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2 LIVE (EFT=l.EFT l2.EFT) GDP SUF URB ,fe r gmm2s first
outreg2 using LIVE.doc, append tstat bdec(3) tdec(2) keep(LIVE EFT GDP SUF URB ) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2 LIVE (EFT=l.EFT l2.EFT) GDP SUF URB EDU OPEN IND,fe r gmm2s first
outreg2 using LIVE.doc, append tstat bdec(3) tdec(2) keep(LIVE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, NO)
quietly  xi: xtivreg2 LIVE (EFT=l.EFT l2.EFT) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first
outreg2 using LIVE.doc, append tstat bdec(3) tdec(2) keep(LIVE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
 quietly   xi: xtivreg2 LIVE (EFT EFT2=l.EFT l2.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first   
  outreg2 using LIVE.doc, append tstat bdec(3) tdec(2) keep(LIVE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE,YES)
   

**PROD支出
quietly xtivreg2 PROD (EFT=l.EFT l2.EFT),fe r gmm2s first
outreg2 using PROD.docx, replace tstat bdec(3) tdec(2) keep(PROD EFT) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2 PROD (EFT=l.EFT l2.EFT) GDP SUF URB ,fe r gmm2s first
outreg2 using PROD.docx, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB ) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2 PROD (EFT=l.EFT l2.EFT) GDP SUF URB EDU OPEN IND,fe r gmm2s first
outreg2 using PROD.docx, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, NO)
quietly  xi: xtivreg2 PROD (EFT=l.EFT l2.EFT) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first
outreg2 using PROD.docx, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
 quietly   xi: xtivreg2 PROD (EFT EFT2=l.EFT l2.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first   
  outreg2 using PROD.docx, append tstat bdec(3) tdec(2) keep(PROD EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE,YES)
   
**减少滞后两期工具变量   
quietly xtivreg2 PROD (EFT=l.EFT),fe r gmm2s first
outreg2 using PROD1.docx, replace tstat bdec(3) tdec(2) keep(PROD EFT) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2 PROD (EFT=l.EFT) GDP SUF URB ,fe r gmm2s first
outreg2 using PROD1.docx, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB ) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2 PROD (EFT=l.EFT) GDP SUF URB EDU OPEN IND,fe r gmm2s first
outreg2 using PROD1.docx, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, NO)
quietly  xi: xtivreg2 PROD (EFT=l.EFT) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first
outreg2 using PROD1.docx, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
 quietly   xi: xtivreg2 PROD (EFT EFT2=l.EFT l.EFT2) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first   
  outreg2 using PROD1.docx, append tstat bdec(3) tdec(2) keep(PROD EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE,YES)   
   
   
   
   
**BPS支出
quietly xtivreg2 BPS (EFT=l.EFT l2.EFT),fe r gmm2s first
outreg2 using  BPS.docx, replace tstat bdec(3) tdec(2) keep( BPS EFT) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2  BPS (EFT=l.EFT l2.EFT) GDP SUF URB ,fe r gmm2s first
outreg2 using  BPS.docx, append tstat bdec(3) tdec(2) keep( BPS EFT GDP SUF URB ) addtext(Province FE, YES,Year FE, NO)
quietly xtivreg2  BPS (EFT=l.EFT l2.EFT) GDP SUF URB EDU OPEN IND,fe r gmm2s first
outreg2 using  BPS.docx, append tstat bdec(3) tdec(2) keep( BPS EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, NO)
quietly  xi: xtivreg2  BPS (EFT=l.EFT l2.EFT) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first
outreg2 using  BPS.docx, append tstat bdec(3) tdec(2) keep( BPS EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
 quietly   xi: xtivreg2 PROD (EFT EFT2=l.EFT l2.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year,fe r gmm2s first   
  outreg2 using PROD.docx, append tstat bdec(3) tdec(2) keep(PROD EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE,YES)
   

//regional heterogeneity---------------------
*两期滞后项
quietly xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r gmm2s first  //东部地区 
outreg2 using RH.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r gmm2s first  //中部地区 
outreg2 using RH.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LGEE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r gmm2s first  //西部地区 U型
outreg2 using RH.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)


quietly xi: xtivreg2 PROD (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r gmm2s first  //东部地区 
outreg2 using RHP.doc, replace tstat bdec(3) tdec(2) keep(PROD EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 PROD (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r gmm2s first  //中部地区 
outreg2 using RHP.doc, append tstat bdec(3) tdec(2) keep(PROD EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 PROD (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r gmm2s first  //西部地区 U型
outreg2 using RHP.doc, append tstat bdec(3) tdec(2) keep(PROD EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)


*注意：prod前期已经经过验证是不存在倒U型关系的
quietly xi: xtivreg2 PROD (EFT =l2.EFT l.EFT) GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r gmm2s first  //东部地区 
outreg2 using RHP.doc, replace tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 PROD (EFT=l2.EFT l.EFT) GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r gmm2s first  //中部地区 
outreg2 using RHP.doc, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 PROD (EFT =l2.EFT l.EFT) GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r gmm2s first  //西部地区 U型
outreg2 using RHP.doc, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)






quietly xi: xtivreg2 LIVE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r gmm2s first  //东部地区 
outreg2 using RHL.doc, replace tstat bdec(3) tdec(2) keep(LIVE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LIVE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r gmm2s first  //中部地区 
outreg2 using RHL.doc, append tstat bdec(3) tdec(2) keep(LIVE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LIVE (EFT EFT2=l2.EFT l.EFT l.EFT2 l2.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r gmm2s first  //西部地区 U型
outreg2 using RHL.doc, append tstat bdec(3) tdec(2) keep(LIVE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)




*一期滞后项
quietly xi: xtivreg2 LGEE (EFT EFT2=l.EFT l.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r gmm2s first  //东部地区 
outreg2 using RH1.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LGEE (EFT EFT2=l.EFT l.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r gmm2s first  //中部地区 
outreg2 using RH1.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LGEE (EFT EFT2=l.EFT l.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r gmm2s first  //西部地区 U型
outreg2 using RH1.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)



quietly xi: xtivreg2 PROD (EFT=l.EFT) GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r gmm2s first  //东部地区 
outreg2 using RH1P.doc, replace tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 PROD (EFT=l.EFT) GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r gmm2s first  //中部地区 
outreg2 using RH1P.doc, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 PROD (EFT=l.EFT) GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r gmm2s first  //西部地区 U型
outreg2 using RH1P.doc, append tstat bdec(3) tdec(2) keep(PROD EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)



quietly xi: xtivreg2 LIVE (EFT EFT2=l.EFT l.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r gmm2s first  //东部地区 
outreg2 using RH1L.doc, replace tstat bdec(3) tdec(2) keep(LIVE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LIVE (EFT EFT2=l.EFT l.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r gmm2s first  //中部地区 
outreg2 using RH1L.doc, append tstat bdec(3) tdec(2) keep(LIVE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtivreg2 LIVE (EFT EFT2=l.EFT l.EFT2) GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r gmm2s first  //西部地区 U型
outreg2 using RH1L.doc, append tstat bdec(3) tdec(2) keep(LIVE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)





*普通xtreg
quietly xi: xtreg LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r   //东部地区 
outreg2 using RH2.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtreg LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r  //中部地区 
outreg2 using RH2.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtreg LGEE EFT EFT2 GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r   //西部地区 U型
outreg2 using RH2.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT EFT2 GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)


quietly xi: xtreg LGEE EFT GDP SUF URB EDU OPEN IND i.year if region=="E" ,fe r   //东部地区 
outreg2 using RH2P.doc, replace tstat bdec(3) tdec(2) keep(LGEE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtreg LGEE EFT GDP SUF URB EDU OPEN IND i.year if region=="M" ,fe r  //中部地区 
outreg2 using RH2P.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)
quietly xi: xtreg LGEE EFT GDP SUF URB EDU OPEN IND i.year if region=="W" ,fe r   //西部地区 U型
outreg2 using RH2P.doc, append tstat bdec(3) tdec(2) keep(LGEE EFT GDP SUF URB EDU OPEN IND) addtext(Province FE, YES,Year FE, YES)


*Robustness check using conserve area 

cd "/Users/xiaoyue/PostPHD/Rcode/LGEE/results"
use NKEFZpanel_1.dta,clear
xtset id year

replace CON = log(10000*国家级自然保护区面积万公顷/年末常住人口万人)  //每万人国家保护区面积，公顷
gen COD = log(化学需氧量排放量万吨*10e6/年末常住) //人均排放COD,g








