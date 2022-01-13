package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

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
    private Integer manufacturerId;

    /**
     * 生产厂商名称
     */
    private String manufacturerName;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
