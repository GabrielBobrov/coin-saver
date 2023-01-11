package com.coinsaver.validation.messages;

public class ErrorMesssages {
	
	public static final String USERALREADYEXISTS = "Já existe um usuário cadastrado com o email informado.";
	public static final String USERNOTFOUND = "Não existe nenhum usuário com o id informado.";
	
	public static String userInvalid(String errors) { return "Os campos informados para o usuário estão inválidos" + errors; }
}
