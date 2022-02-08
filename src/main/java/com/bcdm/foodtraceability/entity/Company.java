package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 企业信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class Company implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 企业ID
     */
    @TableId(type = IdType.AUTO)
    @NotNull(message = "当前企业信息出现错误，请登录后重试", groups = {ModifyGroup.class})
    private Integer companyId;

    /**
     * 创建者用户ID
     */
    @NotNull(message = "当前用户信息出现错误，请登录后重试", groups = {ModifyGroup.class, GetInfoGroup.class})
    private Integer userId;

    /**
     * 企业状态
     */
    private Integer companyStatus;

    /**
     * 企业级别
     */
    private Integer companyLevel;

    /**
     * 企业名称
     */
    @NotBlank(message = "企业名称不能为空", groups = {RegisterGroup.class, ModifyGroup.class})
    @Length(min = 2, max = 40, message = "请输入2-40位的企业名称", groups = {RegisterGroup.class, ModifyGroup.class})
    private String companyName;

    /**
     * 企业联系方式
     */
    @NotBlank(message = "企业的联系方式不能为空", groups = {RegisterGroup.class, ModifyGroup.class})
    private String companyPhone;

    /**
     * 企业图片
     */
    private String companyIcon;

    /**
     * 企业地址
     */
    @NotBlank(message = "企业地址不能为空", groups = {RegisterGroup.class, ModifyGroup.class})
    @Length(min = 4, max = 100, message = "请输入100位以下的企业地址", groups = {RegisterGroup.class, ModifyGroup.class})
    private String companyAddress;

    /**
     * 企业信息
     */
    @NotBlank(message = "企业信息不能为空", groups = {RegisterGroup.class, ModifyGroup.class})
    @Length(min = 4, max = 100, message = "请输入100字以内的企业信息", groups = {RegisterGroup.class, ModifyGroup.class})
    private String companyInfo;

    /**
     * 营业执照
     */
    @NotBlank(message = "请上传营业执照", groups = {RegisterGroup.class, ModifyGroup.class})
    private String businessLicense;

    /**
     * 卫生许可
     */
    private String healthPermit;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
