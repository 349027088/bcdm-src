package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;

import java.time.LocalDateTime;
import java.io.Serializable;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * <p>
 *
 * </p>
 *
 * @author 王
 * @since 2022-02-14
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class CompanyInfoCheck implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 企业ID
     */
    @TableId(value = "company_id", type = IdType.AUTO)
    private Integer companyId;

    /**
     * 申请者ID
     */
    private Integer userId;

    /**
     * 企业名称
     */
    private String companyName;

    /**
     * 企业LOGO
     */
    private String companyIcon;

    /**
     * 联系方式
     */
    private String companyPhone;

    /**
     * 营业执照
     */
    private String businessLicense;

    /**
     * 经营许可证
     */
    private String healthPermit;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
