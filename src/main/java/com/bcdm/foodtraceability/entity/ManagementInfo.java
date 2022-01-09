package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import java.time.LocalDate;
import com.baomidou.mybatisplus.annotation.TableId;
import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 管理员信息
 * </p>
 *
 * @author 王
 * @since 2022-01-09
 */
@Data
  @EqualsAndHashCode(callSuper = false)
    public class ManagementInfo implements Serializable {

    private static final long serialVersionUID=1L;

      /**
     * 管理员ID
     */
        @TableId(value = "manager_id", type = IdType.AUTO)
      private Integer managerId;

      /**
     * 管理员状态
     */
      private Integer managerStatus;

    private Integer managerLevel;

      /**
     * 管理员姓名
     */
      private String managerName;

      /**
     * 身份证号
     */
      private String idCard;

      /**
     * 电话号
     */
      private Integer phoneNumber;

      /**
     * 邮箱地址
     */
      private String mailAddress;

      /**
     * 性别
     */
      private Integer sex;

      /**
     * 年龄
     */
      private Integer age;

      /**
     * 出生日期
     */
      private LocalDate birthday;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
