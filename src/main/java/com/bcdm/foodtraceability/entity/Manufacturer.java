package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotNull;

/**
 * <p>
 * 生产厂商信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class Manufacturer implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 生产厂商ID
     */
    @TableId(type = IdType.AUTO)
    private Integer manufacturerId;

    /**
     * 企业ID
     */
    @NotNull(message = "公司ID不能为空")
    private Integer companyId;

    /**
     * 生产厂商名称
     */
    private String manufacturerName;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
