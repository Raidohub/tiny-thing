package org.amumu.rule.tree.infra.config.boot;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfig {

    @Bean
    public WebMvcConfigurer corsConfigure() {
        return new WebMvcConfigurer() {
            @Override
            public void addCorsMappings(CorsRegistry registry) {
                registry.addMapping("/**")//项目中的所有接口都支持跨域
                        .allowedOriginPatterns("*")//所有地址都可以访问，也可以配置具体地址
                        .allowCredentials(true)
                        .allowedMethods("GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS")
                        .maxAge(3600);// 跨域允许时间
            }
        };
    }
}
