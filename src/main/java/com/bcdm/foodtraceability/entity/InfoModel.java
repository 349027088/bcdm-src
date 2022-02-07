package com.bcdm.foodtraceability.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Map;

/**
 * <p>
 * 返回前端页面的信息载体
 * </p>
 *
 * @author 王
 * @since 2022-02-07
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class InfoModel{

    /**
     * 信息集合體
     */
    private Map<String,Object> infoMap;
}
