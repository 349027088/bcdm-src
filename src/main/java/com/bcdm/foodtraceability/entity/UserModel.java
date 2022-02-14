package com.bcdm.foodtraceability.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * <p>
 * 用户权限信息载体
 * </p>
 *
 * @author 王
 * @since 2022-02-14
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class UserModel {

    /**
     * 用户ID
     */
    private Integer userId;

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
    private String phoneNumber;

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
     * 职位
     */
    private Integer identity;

    /**
     * 职位
     */
    private LocalDateTime jurisdictionUpdateTime;

    /**
     * 出生日期
     */
    private LocalDate birthday;

    /**
     * 通知级别
     */
    private LocalDateTime noticeCheck;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;
}
