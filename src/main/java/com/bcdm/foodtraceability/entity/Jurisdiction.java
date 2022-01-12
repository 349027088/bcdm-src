package com.bcdm.foodtraceability.entity;

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
public class Jurisdiction implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 用户ID
     */
    private Integer userId;

    /**
     * 企业ID
     */
    private Integer companyId;

    /**
     * 供应商ID
     */
    private Integer supplierId;

    /**
     * 职位
     */
    private String identity;

    /**
     * 操作权限
     */
    private Integer jurisdiction;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
