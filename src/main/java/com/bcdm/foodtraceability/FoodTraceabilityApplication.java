package com.bcdm.foodtraceability;

import com.alibaba.fastjson.support.spring.FastJsonHttpMessageConverter;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.http.converter.HttpMessageConverter;

@SpringBootApplication
@MapperScan("com.bcdm.foodtraceability.mapper")
public class FoodTraceabilityApplication {

    public static void main(String[] args) {
        SpringApplication.run(FoodTraceabilityApplication.class, args);
    }

    @Bean
    public HttpMessageConverter fastJsonHttpMessageConverter(){
        return new FastJsonHttpMessageConverter();
    }
}
