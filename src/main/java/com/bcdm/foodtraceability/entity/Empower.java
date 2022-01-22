package com.bcdm.foodtraceability.entity;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 授权信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class Empower implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 授权ID
     */
    @TableId(type = IdType.AUTO)
    private Integer empowerId;

    /**
     * 管理员ID
     */
    private Integer managerId;

    /**
     * 企业ID
     */
    private Integer companyId;

    /**
     * 开始时间
     */
    private LocalDate startDate;

    /**
     * 缴纳金额
     */
    private Integer moneyAmt;

    /**
     * 使用时限
     */
    private Integer usageTime;

    /**
     * 停止服务时间
     */
    private LocalDate serviceStopTime;

    /**
     * 授权凭证
     */
    private String authorizationCertificate;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
