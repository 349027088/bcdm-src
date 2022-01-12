package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
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
public class Supplier implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 供应商ID
     */
    @TableId(value = "supplier_id", type = IdType.AUTO)
    private Integer supplierId;

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
    private Integer supplierPhoneNumber;

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
