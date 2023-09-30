#include <stdio.h>
#include "shell.h"

// 16 bit 立即数 符号扩展
uint32_t sign_ext(uint16_t imm) {
    int32_t signed_imm = *((int16_t*)&imm);
    //printf("imm=0x%8x\n",imm);
    uint32_t extended_imm = *((uint32_t*)&signed_imm);
    //printf("signed_imm=0x%8x\n",signed_imm);
    //printf("extended_imm=0x%8x\n",signed_imm);
    return extended_imm;
}

// 8 bit 符号扩展
uint32_t sign_ext_byte(uint8_t imm) {
    int32_t signed_imm = *((int8_t*)&imm);
    uint32_t extended_imm = *((uint32_t*)&signed_imm);
    return extended_imm;
}

// 零扩展

uint32_t zero_ext_byte(uint8_t imm) { return imm; }

uint32_t zero_ext_half(uint16_t imm) { return imm; }

void pc_add(){
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}



void process_instruction()
{
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. */
    //处理指令，通过指令类型、字段等区分并调用各个指令的处理函数，进行处理
    uint32_t inst;

    // printf("-------------------\n");
    // printf("0x%8x\n",CURRENT_STATE.PC);
    // printf("0x%8x\n",NEXT_STATE.PC);
    // printf("-------------------\n"); 

    inst=mem_read_32(CURRENT_STATE.PC);
    uint8_t op = 0; //指令类型
	uint8_t rs = 0, rt = 0, rd = 0, shamt = 0, funct = 0; //操作数1 2 3
	uint16_t imm = 0;
	uint32_t target = 0;

    /* R:000000(6)+rs(5)+rt(5)+rd(5)+shamt(5)+funct(6)
     * I:op(6)+rs(5)+rt(5)+imm(16)
     * J:op(6)+target(26)
     */

    // 获取指令对应位置的每个操作数
    op = (inst>>26)&0x3F;
    rs = (inst>>21)&0x1F;
    rt = (inst>>16)&0x1F;
    rd = (inst>>11)&0x1F;
    shamt = (inst>>6)&0x1F;
    funct = inst&0x3F;
    imm = inst&0xFFFF;
    target = inst & 0x03FFFFFF;
    printf("op=%x,rs=%x,rt=%x,rd=%x,sa=%x,funct=%x,imm=%d,\n",op,rs,rt,rd,shamt,funct,imm);
    switch (op) {
        case 0x0: {//R
            switch (funct) {
                case 0x0: {
                        // SLL
                        // 按位左移
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
                        pc_add();
                        break;
                    }
                case 0x2: {
                        // SRL
                        // 按位右移
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
                        pc_add();
                        break;
                }
                case 0x3: {
                        // SRA
                        // 算术右移
                        NEXT_STATE.REGS[rd] = (int32_t)CURRENT_STATE.REGS[rt] >> shamt;
                        pc_add();
                        break;
                }
                case 0x4: {
                        //SLLV
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << CURRENT_STATE.REGS[rs];
                        pc_add();
                        break;
                }
                case 0x6: {
                        //SRLV
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> CURRENT_STATE.REGS[rs];
                        pc_add();
                        break;
                }
                case 0x7: {
                        //SRAV
                        NEXT_STATE.REGS[rd] = (int32_t)CURRENT_STATE.REGS[rt] >> CURRENT_STATE.REGS[rs];
                        pc_add();
                        break;
                }
                case 0x8: {
                        //JR
                        NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
                        pc_add();
                        break;
                }
                case 0x9: {
                        // JALR
                        // 将程序计数器（PC）设置为寄存器rs中的值，并将下一条指令的地址存储在寄存器rd中
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4;
                        NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
                        pc_add();
                        break;
                }
                case 0xC: {
                        //SYSCALL
                        switch(CURRENT_STATE.REGS[2]) {
                            case 0xA:
                                RUN_BIT = 0;
                                return;
                        }
                        break;
                }
                case 0x10: {
                        //MFHI (move from HI)
                        // 取高位存入寄存器
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.HI;
                        pc_add();
                        break;
                }
                case 0x11: {
                        //MTHI (move to HI)
                        NEXT_STATE.HI = CURRENT_STATE.REGS[rs];
                        pc_add();
                        break;
                }

                case 0x12: {
                        //MFLO
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.LO;
                        pc_add();
                        break;
                }

                case 0x13: {
                        //MTLO
                        NEXT_STATE.LO = CURRENT_STATE.REGS[rs];
                        pc_add();
                        break;
                }

                case 0x18: {
                        //MULT
                        int64_t result = (int64_t)((int32_t)CURRENT_STATE.REGS[rs]) * (int64_t)((int32_t)CURRENT_STATE.REGS[rt]);
                        NEXT_STATE.HI = (result >> 32) & 0xFFFFFFFF;
                        NEXT_STATE.LO = result & 0xFFFFFFFF;
                        pc_add();
                        break;
                }
                case 0x19: {
                        //MULTU
                        uint64_t result = (uint64_t)CURRENT_STATE.REGS[rs] * (uint64_t)CURRENT_STATE.REGS[rt];
                        NEXT_STATE.HI = (result >> 32) & 0xFFFFFFFF;
                        NEXT_STATE.LO = result & 0xFFFFFFFF;
                        pc_add();
                        break;
                }
                case 0x1A: {
                        //DIV
                        int32_t divisor = (int32_t)CURRENT_STATE.REGS[rt];
                        if (divisor != 0) {
                            int32_t dividend = (int32_t)CURRENT_STATE.REGS[rs];
                            NEXT_STATE.LO = dividend / divisor;
                            NEXT_STATE.HI = dividend % divisor;
                        }
                        pc_add();
                        break;
                }
                case 0x1B: {
                        //DIVU
                        uint32_t divisor = CURRENT_STATE.REGS[rt];
                        if (divisor != 0) {
                            uint32_t dividend = CURRENT_STATE.REGS[rs];
                            NEXT_STATE.LO = dividend / divisor;
                            NEXT_STATE.HI = dividend % divisor;
                        }
                        pc_add();
                        break;
                }


                case 0x20: {
                        //ADD
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
                        //printf("0x%8x\n",CURRENT_STATE.PC);
                        pc_add();
                        //printf("0x%8x\n",NEXT_STATE.PC);
                        break;
                }
                case 0x21: {
                        //ADDU
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
                        pc_add();
                        break;
                }
                case 0x22: {
                        //SUB
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
                        pc_add();
                        break;
                }
                case 0x23: {
                        //SUBU
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
                        pc_add();
                        break;
                }
                case 0x24: {
                        //AND
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] & CURRENT_STATE.REGS[rt];
                        pc_add();
                        break;
                }
                case 0x25: {
                        //OR
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt];
                        pc_add();
                        break;
                }
                case 0x26: {
                        //XOR
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt];
                        pc_add();
                        break;
                }
                case 0x27: {
                        //NOR
                        NEXT_STATE.REGS[rd] = ~(CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt]);
                        pc_add();
                        break;
                }
                case 0x2A: {
                        //SLT
                        NEXT_STATE.REGS[rd] = (int32_t)CURRENT_STATE.REGS[rs] < (int32_t)CURRENT_STATE.REGS[rt] ? 1 : 0;
                        pc_add();
                        break;
                }
                case 0x2B: {
                        //SLTU
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt] ? 1 : 0;
                        pc_add();
                        break;
                }

                default: {
                    printf("Unknown R-type instruction: 0x%x\n", inst);
                    break;
                }
            }
            break;
        }
            
        case 0x1: {
            switch (rt){
                case 0x0: { // BLTZ
                    // 如果rs寄存器中的值小于零，则分支到目标地址
                    if ((int32_t)CURRENT_STATE.REGS[rs] < 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + (sign_ext(imm) << 2);
                    } else {
                        pc_add();
                    }
                    break;
                }
                case 0x1: { // BGEZ
                    // 如果rs寄存器中的值大于或等于零，则分支到目标地址
                    if ((int32_t)CURRENT_STATE.REGS[rs] >= 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + (sign_ext(imm) << 2);
                    } else {
                        pc_add();
                    }
                    break;
                }
                case 0x10: { // BLTZAL
                    // 如果rs寄存器中的值小于零，则分支到目标地址，并将下一条指令的地址存储在寄存器31（ra寄存器）中
                    if ((int32_t)CURRENT_STATE.REGS[rs] < 0) {
                        NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                        NEXT_STATE.PC = CURRENT_STATE.PC + (sign_ext(imm) << 2);
                    } else {
                        pc_add();
                    }
                    break;
                }
                case 0x11: { // BGEZAL
                    // 如果rs寄存器中的值大于或等于零，则分支到目标地址，并将下一条指令的地址存储在寄存器31（ra寄存器）中
                    if ((int32_t)CURRENT_STATE.REGS[rs] >= 0) {
                        NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                        NEXT_STATE.PC = CURRENT_STATE.PC + (sign_ext(imm) << 2);
                    } else {
                        pc_add();
                    }
                    break;
                }




            }
            break;
        }

        case 0x2: { // J
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xF0000000) | (target << 2);
            break;
        }
        case 0x3: { // JAL
            NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xF0000000) | (target << 2);
            pc_add();
            break;
        }
        case 0x4: { // BEQ
            //printf("rs=0x%8x\n",CURRENT_STATE.REGS[rs]);
            //printf("rt=0x%8x\n",CURRENT_STATE.REGS[rt]);
            if (CURRENT_STATE.REGS[rs] == CURRENT_STATE.REGS[rt]) {
                //printf("current_pc=0x%8x\n",CURRENT_STATE.PC);
                //printf("offset=0x%8x\n",sign_ext(imm) << 2);
                //printf("next_pc=0x%8x\n",NEXT_STATE.PC);
                NEXT_STATE.PC = CURRENT_STATE.PC + (sign_ext(imm) << 2);
                //printf("next_pc=0x%8x\n",NEXT_STATE.PC);
            } else {
                pc_add();
            }
            
            
            break;
        }
        case 0x5: { // BNE
            if (CURRENT_STATE.REGS[rs] != CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + (sign_ext(imm) << 2);
            } else {
                pc_add();
            }
            break;
        }
        case 0x6: { // BLEZ
            if ((int32_t)CURRENT_STATE.REGS[rs] <= 0) {
                NEXT_STATE.PC = CURRENT_STATE.PC + (sign_ext(imm) << 2) + 4;
            } else {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            }
            break;
        }
        case 0x7: { // BGTZ
            // 如果rs寄存器中的值大于零，分支到目标地址
            if ((int32_t)CURRENT_STATE.REGS[rs] > 0) {
                NEXT_STATE.PC = CURRENT_STATE.PC + (sign_ext(imm) << 2);
            } else {
                pc_add();
            }
            break;
        }
        case 0x8: { // ADDI
            // 将寄存器rs中的值与立即数imm相加，结果存储在寄存器rt中
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + imm;
            pc_add();
            break;
        }
        case 0x9: { // ADDIU
            // 将寄存器rs中的值与无符号立即数imm相加，结果存储在寄存器rt中
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + imm;

            pc_add();
            break;
        }
        case 0xA: { // SLTI
            // 如果rs寄存器中的值小于立即数imm，将1存储在寄存器rt中，否则存储0
            NEXT_STATE.REGS[rt] = (int32_t)CURRENT_STATE.REGS[rs] < imm ? 1 : 0;
            pc_add();
            break;
        }
        case 0xB: { // SLTIU
            // 如果rs寄存器中的值小于无符号立即数imm，将1存储在寄存器rt中，否则存储0
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] < (uint32_t)imm ? 1 : 0;
            pc_add();
            break;
        }
        case 0xC: { // ANDI
            // 立即数按位与
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] & imm;
            pc_add();
            break;
        }
        case 0xD: { // ORI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] | imm;
            pc_add();
            break;
        }
        case 0xE: { // XORI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] ^ imm;
            pc_add();
            break;
        }
        case 0xF: { // LUI
            // 立即数imm左移16位，存储在寄存器rt的高16位中
            NEXT_STATE.REGS[rt] = imm << 16;
            pc_add();
            break;
        }
        case 0x20: { // LB
            // 从内存中加载一个字节（8位），并将其符号扩展为32位，然后存储在寄存器rt中
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xff;

            NEXT_STATE.REGS[rt] = sign_ext_byte(byte);

            pc_add();
            break;
        }
        case 0x21: { // LH
            // 从内存中加载一个半字（16位），并将其符号扩展为32位，然后存储在寄存器rt中
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xffff;

            NEXT_STATE.REGS[rt] = sign_ext(byte);

            pc_add();
            break;
        }
        case 0x23: { // LW
            // 从内存中加载一个字（32位），然后存储在寄存器rt中
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];
            //printf("lw_addr= 0x%8x\n",addr);
            NEXT_STATE.REGS[rt] = mem_read_32(addr);
            //printf("rt= 0x%8x\n",NEXT_STATE.REGS[rt]);
            pc_add();
            break;
        }
        case 0x24: { // LBU
            // 从内存中加载一个字节（8位），并将其零扩展为32位，然后存储在寄存器rt中
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xff;

            NEXT_STATE.REGS[rt] = zero_ext_byte(byte);
            pc_add();
            break;
        }
        case 0x25: { // LHU
            // 从内存中加载一个半字（16位），并将其零扩展为32位，然后存储在寄存器rt中
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint16_t half = mem_read_32(addr) & 0xffff;

            NEXT_STATE.REGS[rt] = zero_ext_half(half);

            pc_add();
            break;
        }
        case 0x28: { // SB
            // 将寄存器rt中的最低字节存储到内存中的指定地址
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint32_t val = (mem_read_32(addr) & 0xffffff00) | (CURRENT_STATE.REGS[rt] & 0xff);

            mem_write_32(addr, val);

            pc_add();
            break;
        }
        case 0x29: { // SH
            // 将寄存器rt中的最低两个字节存储到内存中的指定地址
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint32_t val = (mem_read_32(addr) & 0xffff0000) |(CURRENT_STATE.REGS[rt] & 0xffff);
            
            mem_write_32(addr, val);

            pc_add();
            break;
        }
        case 0x2B: { // SW
            // 将寄存器rt中的一个字（32位）存储到内存中的指定地址
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];
            //printf("sw_addr= 0x%8x\n",addr);
            mem_write_32(addr, CURRENT_STATE.REGS[rt]);
            pc_add();
            break;
        }
    }
}
