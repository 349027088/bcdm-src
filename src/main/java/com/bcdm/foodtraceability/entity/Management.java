package com.bcdm.foodtraceability.entity;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.validator.constraints.Email;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.*;

/**
 * <p>
 * 管理员信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class Management implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 管理员ID
     */
    @TableId(type = IdType.AUTO)
    private Integer managerId;

    /**
     * 管理员账号
     */
    @NotNull(message = "账号不能为空")
    @Length(max=32,min=6,message="用户长度为6-32位")
    private String loginId;

    /**
     * 管理员密码
     */
    @NotNull(message = "密码不能为空")
    @Length(max=16,min=6,message="密码长度为6-16位")
    private String password;

    /**
     * UUID
     */
    private String salt;

    /**
     * 用户状态
     */
    private Integer managerStatus;

    /**
     * 管理级别
     */
    private Integer managerLevel;

    /**
     * 管理员姓名
     */
    @NotNull(message = "姓名不能为空",groups = {RegisterGroup.class})
    private String managerName;

    /**
     * 身份证号
     */
    @NotNull(message = "请输入正确的身份证号",groups = {RegisterGroup.class})
    @Length(max=18,min=15,message="请输入正确的身份证号",groups = {RegisterGroup.class})
    private String idCard;

    /**
     * 电话号
     */
    @NotNull(message = "请输入正确的电话号码",groups = {RegisterGroup.class})
    @Pattern(message = "请输入正确的电话号码", regexp = "^[1](([3-9][0-9])|([4][5-9])|([5][0-3,5-9])|([6][5,6])|([7][0-8])|([8][0-9])|([9][1,8,9]))[0-9]{8}$",groups = {RegisterGroup.class})
    private String phoneNumber;

    /**
     * 邮箱地址
     */
    @Email(message = "请输入正确的邮箱地址",groups = {RegisterGroup.class})
    private String mailAddress;

    /**
     * 性别
     */
    @NotNull(message = "请正确选择性别",groups = {RegisterGroup.class})
    @Min(value = 0,message = "请正确选择性别",groups = {RegisterGroup.class})
    @Max(value = 1,message = "请正确选择性别",groups = {RegisterGroup.class})
    private Integer sex;

    /**
     * 年龄
     */
    @Min(value = 1,message = "请输入正确的年龄",groups = {RegisterGroup.class})
    @Max(value = 150,message = "请输入正确的年龄",groups = {RegisterGroup.class})
    private Integer age;

    /**
     * 出生日期
     */
    private LocalDate birthday;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
