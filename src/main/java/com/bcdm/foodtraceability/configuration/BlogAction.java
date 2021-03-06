package com.bcdm.foodtraceability.configuration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BlogAction{
    //定义一个全局的记录器，通过LoggerFactory获取
    public static final Logger logger = LoggerFactory.getLogger(BlogAction.class);

    public static void main(String[] args) {
        logger.info("logback 成功了");
        logger.error("logback 成功了");
    }
}