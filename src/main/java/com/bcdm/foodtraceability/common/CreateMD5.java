package com.bcdm.foodtraceability.common;

import org.springframework.stereotype.Component;
import org.springframework.util.DigestUtils;

import java.nio.charset.StandardCharsets;

/**
 * <p>
 *  工具类：使用MD5进行加密
 * </p>
 *
 * @author 王
 * @since 2022-01-11
 */
@Component
public class CreateMD5 {
    /**
     * MD5加密
     * @param str 需要加密的字符串
     * @return 加密后的字符串
     */
    public static String Md5encode(String str) {
        return DigestUtils.md5DigestAsHex(str.getBytes());
    }
}
