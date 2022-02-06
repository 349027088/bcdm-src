package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.DeleteGroup;
import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

/**
 * <p>
 * 供应商信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class Supplier implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 供应商ID
     */
    @TableId(value = "supplier_id", type = IdType.AUTO)
    @NotNull(message = "当前供应商状态异常请刷新重试", groups = {DeleteGroup.class, ModifyGroup.class})
    private Integer supplierId;

    /**
     * 企业ID
     */
    @NotNull(message = "公司ID不能为空", groups = {CreateGroup.class, GetInfoGroup.class, ModifyGroup.class, DeleteGroup.class})
    private Integer companyId;

    /**
     * 供应商状态
     */
    private Integer supplierStatus;

    /**
     * 供应商级别
     */
    private Integer supplierLevel;

    /**
     * 供应商名称
     */
    @NotBlank(message = "供应商名称不能为空", groups = {CreateGroup.class, ModifyGroup.class})
    @Length(min = 4, max = 40, message = "请输入4-40位的正确供应商名称")
    private String supplierName;

    /**
     * 联系方式
     */
    @NotBlank(message = "电话号码不能为空", groups = {CreateGroup.class, ModifyGroup.class})
    @Pattern(message = "请输入正确的电话号码", regexp = "^[1](([3][0-9])|([4][5-9])|([5][0-3,5-9])|([6][5,6])|([7][0-8])|([8][0-9])|([9][1,8,9]))[0-9]{8}$", groups = {CreateGroup.class, ModifyGroup.class})
    private String supplierPhoneNumber;

    /**
     * 主营业务
     */
    private String mainBusiness;

    /**
     * 营业执照
     */
    private String businessLicense;

    /**
     * 卫生许可
     */
    private String healthPermit;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;

}
