package com.coinsaver;

import com.coinsaver.core.io.Base64ProtocolResolver;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.util.TimeZone;

@SpringBootApplication
public class CoinSaverApiApplication {

	public static void main(String[] args) {
		TimeZone.setDefault(TimeZone.getTimeZone("UTC"));

		var app = new SpringApplication(CoinSaverApiApplication.class);
		app.addListeners(new Base64ProtocolResolver());
		app.run(args);
	}

}
