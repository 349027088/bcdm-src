package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.DeleteGroup;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 商品类别信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class GoodsType implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 商品类别ID
     */
    @TableId(type = IdType.AUTO)
    private Integer goodsTypeId;

    /**
     * 商品类别名称
     */
    @Length(min = 1, max = 40, message = "商品种类名称长度请控制在40以下")
    @NotBlank(message = "商品种类名称不能为空")
    private String goodsTypeName;

    /**
     * 企业ID
     */
    @NotNull(message = "当年公司信息异常，请重新登录后再试", groups = {DeleteGroup.class, CreateGroup.class})
    private Integer companyId;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
