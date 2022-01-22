package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

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
@Data
@EqualsAndHashCode(callSuper = false)
public class Supplier implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 供应商ID
     */
    @TableId(value = "supplier_id", type = IdType.AUTO)
    private Integer supplierId;

    /**
     * 企业ID
     */
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
     * 联系方式
     */
    @NotNull(message = "请输入正确的电话号码")
    @Pattern(message = "请输入正确的电话号码", regexp = "^[1](([3][0-9])|([4][5-9])|([5][0-3,5-9])|([6][5,6])|([7][0-8])|([8][0-9])|([9][1,8,9]))[0-9]{8}$")
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
