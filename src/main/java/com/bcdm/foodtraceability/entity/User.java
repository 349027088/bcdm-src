package com.bcdm.foodtraceability.entity;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class User implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 用户ID
     */
    private Integer userId;

    /**
     * 账号
     */
    private String loginId;

    /**
     * 密码
     */
    private String password;

    /**
     * UUID
     */
    private String salt;

    /**
     * 用户状态
     */
    private Integer userStatus;

    /**
     * 姓名
     */
    private String userName;

    /**
     * 身份证号
     */
    private String idCard;

    /**
     * 电话号
     */
    private Integer phoneNumber;

    /**
     * 邮箱地址
     */
    private String mailAddress;

    /**
     * 性别
     */
    private Integer sex;

    /**
     * 年龄
     */
    private Integer age;

    /**
     * 出生日期
     */
    private LocalDate birthday;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
