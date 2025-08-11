package com.dg.bloodcenter.ui;

import java.util.Scanner;

public class Menu5 {
	public static int DeleteMenu(Scanner s) {
		System.out.println("---------------------------------");
		System.out.println("폐기사유 항목 선택");
		System.out.println("---------------------------------");
		System.out.println("1.해당없음");
		System.out.println("2.급성 질병");
		System.out.println("3.감염발생 지역방문");
		System.out.println("4.기타사유");
		System.out.println("---------------------------------");
		System.out.println("항목 선택:");
		int m = s.nextInt();
		return m;
	}
}
