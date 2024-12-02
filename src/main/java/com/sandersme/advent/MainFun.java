package com.sandersme.advent;

import java.util.List;
import java.util.stream.Collectors;

class MainFun {

	public static void main(String[] args) {
		System.out.println("HELLO WORLD");
		String f = "Hello";
		f.toString();
		var list = List.of(1, 2,3 , 4);

		list.stream().map(a -> a + 5).collect(Collectors.toList()).forEach(System.out::println);
	
	}


} 
