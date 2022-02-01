package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.validator.constraints.Email;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import java.time.LocalDate;
import java.time.LocalDateTime;


@Getter
@Setter
public class UserModel {

    /**
     * 用户ID
     */
    @TableId(type = IdType.AUTO)
    private Integer userId;

    /**
     * 用户状态
     */
    private Integer userStatus;

    /**
     * 姓名
     */
    @NotNull(message = "姓名不能为空",groups = {RegisterGroup.class})
    private String userName;

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

    private LocalDateTime createTime;

    private LocalDateTime updateTime;
}
