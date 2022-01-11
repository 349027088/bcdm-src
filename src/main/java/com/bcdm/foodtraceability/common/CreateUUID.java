package com.bcdm.foodtraceability.common;

import org.springframework.stereotype.Component;

import java.util.UUID;

/**
 * <p>
 *  工具类：生成一个随机的盐
 * </p>
 *
 * @author 王
 * @since 2022-01-11
 */
@Component
public class CreateUUID {

    /**
     * 无符号UUID生成
     * @return 随机生成的盐
     */
    public static String getUUID(){
        return UUID.randomUUID().toString().replaceAll("-","");
    }
}
