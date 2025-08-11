package com.dg.bloodcenter.util;

import com.dg.bloodcenter.db.DbManager;
import com.dg.bloodcenter.model.Donor;

public class SKTestMain {

	public static void main(String[] args) throws Exception {

		RandData r= new RandData();
		DbManager ndb= new DbManager();
		
		//전혈 헌혈은 8주 후, 성분 헌혈은 2주 후 (나중에 되면 더 추가하기)
		
		
		System.out.print("생성할 사람의 수를 입력하세요");
		int n=10;
		
		String[] arrlist = new String[n];
		String[] randtel = new String[n];
		arrlist= r.getRandRr(n);
		randtel = r.getRandTel(n);
		for(int i=0;i<n;i++) {
			Donor dn = new Donor(ndb.getdid(),r.getRandName2() , 
					arrlist[i],r.getGender2(arrlist[i]) , r.getAddress2(), 
					randtel[i], r.getBloodType2());
			ndb.insertDonor(dn);
		}
		
		//{"전혈","혈소판 성분헌혈","혈장 성분헌혈"};
		System.out.print("생성할 예약 수를 입력하세요");
		int n2=2;
		for(int i=0;i<n2;i++) {
			String bookid = ndb.getNextBookingId();	//부킹id
			String centerid = r.getRandCenterId();	//센터id
			String booktime = r.getRandBookTime();
			String dotype =  r.getRandDonationType();
			//book: 부킹id, 센터id, donorid, donationtype, bloodvolume, booktime, interview, gift
			ndb.RandBookingFull(bookid,centerid, r.getRandDonorId(n), dotype, r.getBloodVolume(dotype), booktime,
				"헌혈가능", r.getRandGift());
				if(dotype == "전혈") {
					//도네id검색한 다음, 
				}
			
			//keep: bloodkey, 부킹id, 센터id, bankid, stdate, enddate, comment
			ndb.addBloodKeep(bookid, centerid, "BK01", booktime, null);
		}	
		
		
		
		
		
		
		
		
		
	System.out.println("---------------");
//	ndb.showbloodkeep();
	//r.getDonationType();
//	String test1 = r.getRandDonationType();
//	System.out.println(test1 + r.getBloodVolume(test1));
//	System.out.println(r.getRandDonorId());
//	System.out.println(r.getRandGift());
//	
//	System.out.println((int)(Math.random()*3)-1);
	
	//랜덤날짜 쓰는 방법
//	System.out.println(r.getRandBookTime());
	
//	for(int i=0;i<8;i++) {
//		String dotype =  r.getRandDonationType();
//		ndb.RandBookingFull(r.getRandCenterId(), r.getRandDonorId(), dotype, r.getBloodVolume(dotype), r.getRandBookTime(),
//				"헌혈가능", r.getRandGift());
//	}
	
	//--------------------------------------------------------------	
//		arrlist= r.getRandRr(n);
//		randtel = r.getRandTel(n);
//		for(int i=0;i<n;i++) {
//			Donor dn = new Donor(ndb.getdid(),r.getRandName2() , 
//					arrlist[i],r.getGender2(arrlist[i]) , r.getAddress2(), 
//					randtel[i], r.getBloodType2());
//			ndb.insertDonor(dn);
//		}
//		
//	ndb.dbClose();
//	

//	System.out.println(ndb.getdid());
//	System.out.println(r.getRandName2());
//	System.out.println(arrlist[0]);
//	System.out.println(r.getGender2(arrlist[0]));
//	System.out.println(r.getAddress2());
//	System.out.println(randtel[0]);
//	System.out.println(r.getBloodType2());
	

	
	//--------------------------------------------------------------		

	
		//전화번호 만들기
//		for(String i:R.getRandTel(n)) {
//			System.out.println(i);
//		}
//--------------------------------------------------------------		
		//이름 만들기
//		for(String i:R.getRandName(n)) {
//			System.out.println(i);
//		}
//--------------------------------------------------------------		
		//주민등록번호 만들기
//		for(int i=0;i<n;i++) {
//			arrlist[i]=R.getRandRr(n)[i];
//			System.out.println(arrlist[i]);
//		}
//		
//--------------------------------------------------------------		
//		//성별
//		for(char i:R.getGender(arrlist, n)) {
//			System.out.println(i);
//		}
		
//--------------------------------------------------------------		
		//주소
//		for(String i:R.getAddress(n)) {
//			System.out.println(i);
//		}
		
		//혈액형
//		System.out.println("---------------");
//
//		for(String i:R.getBloodType(n)) {
//			System.out.println(i);
//		}
//		
		

	
		
	
		
		
		
	}

}
