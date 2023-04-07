package com.coinsaver.core.springdoc;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.tags.Tag;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Arrays;
import java.util.Arrays;
import java.util.Collections;
@Configuration
public class SpringDocConfig {

    @Bean
    public OpenAPI openAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("CoinSaver API")
                        .version("v1")
                        .description("REST API do CoinSaver")
                        .license(new License()
                                .name("Apache 2.0")
                                .url("http://springdoc.com")
                        )
                ).tags(Arrays.asList(
                        new Tag().name("Transactions").description("Gerencia de transações"),
                        new Tag().name("Authentication").description("Gerencia de autenticações"),
                        new Tag().name("Divisions").description("Gerencia de divisões"))
                ).components(new Components());
    }
}
