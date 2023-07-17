*****************************************************************************************************
* 
* SP-6015 Metode Kuantitatif Untuk Policy Analysis
* Hands-on On Hierarchical Linear Model (HLM)
* By Adenantera Dwicaksono, PhD.
*
*****************************************************************************************************

***************************************
* 1. Setting up working environment
***************************************

set more off
version 12

//set working directory
cd "D:\Documents\OneDrive - Institut Teknologi Bandung\Documents\Teaching\SP6015\Mgg#10"

***************************************
* 2. Loading the dataset
***************************************

																								/*
Syarat utama dari HLM adalah bahwa struktur datanya bersifat bertingkat: 
variable lvl-1 bersarang dalam variabel lvl-2 dan bersarang dalam variabel lvl-3, dst. 
Kita akan mengawali dengan membuka dataset yang telah bersuktrut untuk mendemostrasikan 
entuk struktur data yang akan digunalan dalam analisis HLM. 									*/

//Load the dataset (Final dataset)
use hsb.dta, clear

describe
																					/*
********High School and Beyond***********

Data ini merupakan sub-sampel dari Survey High School and Beyond (1982) yang juga digunakan pada 
buku Hierarchical Linear Models oleh Raudenbush and Bryk. 
File dataset ini adalah hsb.dta yang berisikan 7185 siswa tingkat sekolah menengah atas di USA 
yang "bersarang"/nested di 160 sekolah. Dataset ini berisikan variable-variabel:
- schid: kode sekolah
- minority: siswa adalah termasuk minoritas (0=kulit putih, 1= non-kulit putih)
- female: siswa perempuan (0=laki-laki, 1= perempuan)
- ses: index/score status sosial ekonomi dari siswa
- mathach: nilai siswa pada test matematika
- size: jumlah siswa di sekolah
- schtype: tipe sekolah (1= sekolah swasta, 0 = sekolah negeri)
- meanses: rata-rata skor SES untuk setiap sekolah

Variable-variabel diatas terdiri dari 2 dataset. Dataset pertama adalah dataset untuk level 
individu siswa (hsb_student.dta) atau yang akan disebut seterusnya sebagai Level-1 dan dataset 
yang kedua adalah (hsb_school.dta), yang selanjutnya akan disebut sebagai Level-2. 
Dataset hsb_student.dta berisikan informasi tentang 7185 siswa, sedangkan hsb_school.dta 
berisikan informasi tentang 160 sekolah, dimana seluruh sampel siswa pada dataset hsb_student.dta 
bersekolah. Kedua dataset tersebut akan disatukan (merged-up) menjadi 1 dataset utuh.

Selanjutnya kita akan membuka dataset level 1 dan level 2 secara terpisah dan 
kemudian disatukan (merge) menjadi 1 dataset berstruktur hierarkis.								*/

//Buka dataset Lvl-1
use "hsb_student.dta", clear
describe
sum

//Buka dataset level-2
use "hsb_school.dta", clear
describe
sum

// Merging the dataset level-1 ke dataset level-2
merge 1:m schid using "hsb_student.dta",  nogen

***************************************
* 3. Eksplorasi Data
***************************************

// a. Statistika deskriptif
sum

// b. Informasi tentang cluster
sum mathach

scalar om = r(mean)

bysort schid: gen sn = _N     // school n

by schid: gen first = _n == 1 // mark first obs in school

sum sn if first

// c. Visualisasi data
twoway (scatter mathach ses if schtype ==1, mcolor(blue) msize(small) ///
		msymbol(circle_hollow) mlwidth(vthin)) (scatter mathach ses if schtype ==0, mcolor(green) ///
		msize(vsmall) msymbol(square_hollow) mlwidth(vvvthin)), ytitle(Student's Math Score) 	///
		legend(order(1 "Catholic" 2 "Public"))

***************************************
* 3. Hierarchical Linear Model:
***************************************

****************
*Model 1: One-way ANOVA dengan Random Effects: Empty Model
****************

//Jalankan model
mixed mathach meanses || schid: , variance reml

//Check intraclass correlation
estat icc


****************
*Model 2: Regresi dengan Means-as-Outcomes
****************

//jalankan model
mixed mathach meanses || schid: , variance reml

//Check intraclass correlation
estat icc

*****************
* Model 3: Model Random Koefisien
*****************

//Create centered variables : school-mean centering (group-centering)
gen grp_cent_ses = ses - meanses

sum  ses grp_cent_ses

//Jalankan model
mixed mathach grp_cent_ses || schid: grp_cent_ses, cov(un) reml // Jalankan mixed model

//Check intraclass correlation
estat icc

*****************
* Model 4: Model Intercept and Slopes as Outcomes
*****************

//Jalankan model
mixed mathach meanses schtype grp_cent_ses 	///
	  c.meanses#c.grp_cent_ses i.schtype#c.grp_cent_ses || ///
	  schid: grp_cent_ses , reml cov(un)
	  
//Check intraclass correlation
estat icc

***************************************
* 4. How to Decide Centering Independent Variables
***************************************

/*
Yang dimaksud dengan "centering" dalam analisis HLM adalah merubah nilai sedemikian rupa 
untuk memperbaiki interpretasi model. Terdapat 3 jenis centering dalam analisis HLM:
- uncentered  (𝑋𝑖𝑗)  : variabel dipertahankan pada nilai aslinya. 
  Interpretasinya bahwa zero = 0
- cluster/group-mean centering  (𝑋𝑖𝑗−𝑋¯.𝑗)  : Variabel dirubah sedemikian rupa sehingga 
  menunjukkan perbedaan antara individu dengan groupnya. 
  Interpretasi zero = group mean
- grand-mean centering  (𝑋𝑖𝑗−𝑋¯¯..) : variabel dirubah sedemikin rupa sehingga 
  menunjukkan perbedaa individu dengan nilai rata-rata keseluruhan populasi. 
  Interpretasinya zero = overall mean

Konsekuensi Centering:
- dapat merubah interpretasi dari intercept pada level 1
- dapat mempengaruhi estimasi dari slopes
*/

gen ses_uncentered = ses //uncentered

egen gmses=mean(ses), by(schid) //group-mean centered
gen ses_group_ctrd = ses - gmses

egen grandmses = mean(gmses) //grand-mean centered
gen ses_grand_ctrd = ses - grandmses

sum ses_uncentered ses_group_ctrd ses_grand_ctrd

