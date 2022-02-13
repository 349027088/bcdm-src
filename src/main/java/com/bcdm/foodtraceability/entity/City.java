package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;

import java.io.Serializable;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * <p>
 * 城市信息载体Entity
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class City implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 城市编号
     */
    @TableId(type = IdType.AUTO)
    private Integer cityId;

    /**
     * 城市名称
     */
    private String cityName;

    /**
     * 省编号
     */
    private Integer proId;

    /**
     * 省份名称
     */
    private String proName;


}
