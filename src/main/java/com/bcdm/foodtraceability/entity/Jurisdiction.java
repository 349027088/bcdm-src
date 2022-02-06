package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * <p>
 * 用户企业管理信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class Jurisdiction implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 用户ID
     */
    @TableId(type = IdType.AUTO)
    private Integer userId;

    /**
     * 企业ID
     */
    private Integer companyId;

    /**
     * 职位
     */
    private Integer identity;

    /**
     * 操作权限
     */
    private Integer jurisdiction;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
