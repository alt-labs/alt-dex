import styled, { keyframes } from 'styled-components';
import {
  Dialog as DialogCommon,
  DialogBlock as DialogBlockCommon,
  DialogSubmitButton,
} from '../styles';

export const Dialog = styled(DialogCommon)`
  max-width: 664px;
`;

export const DialogBlock = styled(DialogBlockCommon)`
  flex-direction: row;
  padding: 10px 10px;
  align-items: center;
`;

export const InputWrapper = styled.div`
  padding: 0 10px;
  width: 100%;
  display: flex;
`;

export const AnotherInputWrapper = styled.div`
  padding: 10px;
  background: #14142b;
  border-radius: 8px;
  width: 100%;
  display: flex;
  align-items: center;
`;

export const TokenAmountInput = styled.input`
  background: transparent;
  border: none;
  outline: none;
  display: flex;
  color: #fcfcfc;
  position: relative;
  font-weight: 500;
  padding: 0 0 0 5px;
  flex: 1 1 auto;
  font-size: 14px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
`;

export const MaxButton = styled.button`
  height: 28px;
  cursor: pointer;
  margin-right: 0.5rem;
  outline: none !important;

  background: transparent;
  border: 1px solid #a0a3bd !important;
  font-weight: 500;
  font-size: 12px;
  color: #a0a3bd;
  border-radius: 4px;
  width: 40px;

  &:hover {
    border-color: rgb(0, 51, 173);
  }
`;

export const SwapArrowBlock = styled.div`
  position: relative;
  padding: 0.5rem;
  display: block;
  text-align: center;
  z-index: 2;
`;

export const SwapArrowWrapper = styled.div`
  border-radius: 40px;
  outline: none !important;
  border: none;
  /*position: absolute;*/
  /*top: -15px;*/
  /*right: 0;*/
  /*left: 10px;*/
  background: #14142b;
  padding: 0;
  width: 45px;
  height: 45px;
  display: flex;
  align-items: center;
  justify-content: center;
  margin: auto;
`;

export const TildaWrapper = styled.div`
  font-size: 22px;
  color: #f7f7f7;
  width: 40px;
  margin-right: 8px;
  text-align: center;
`;

export const DialogFooter = styled.div`
  padding: 3rem 0 2rem 0;
`;

export const Modal = styled.div<{ isActive: boolean }>`
  position: absolute;
  top: 20px;
  right: 20px;
  bottom: 20px;
  left: 20px;
  overflow: auto;
  background-color: #262338;
  z-index: 4;
  border-radius: 24px;
  opacity: 0;
  visibility: hidden;
  transition: all 200ms;
  display: flex;
  flex-direction: column;
  flex: 1 1 auto;

  ${({ isActive }) =>
    isActive &&
    `
  opacity: 1;
  visibility: visible;
  `}
`;

export const ModalContent = styled.div<{ shouldCenterText?: boolean }>`
  padding: 40px;
  position: relative;
  color: #f7f7f7;
  display: flex;
  flex-direction: column;
  flex: 1 1 auto;

  ${({ shouldCenterText }) =>
    shouldCenterText &&
    `
  text-align: center;
  `}
`;

export const CloseModalButton = styled.button`
  cursor: pointer;
  margin-top: auto;
  position: absolute;
  top: 20px;
  right: 20px;
  color: #f7f7f7;
  border: 0;
  outline: 0;
  box-shadow: none;
  background: transparent;
`;

export const ModalAmount = styled.h2`
  margin-top: auto;
  margin-bottom: 1.5rem;
`;

export const ModalSubmitButton = styled(DialogSubmitButton)`
  margin-top: auto;
`;

const hourglassAnimation = keyframes`
  0% {
    transform: rotate(0);
    animation-timing-function: cubic-bezier(0.55, 0.055, 0.675, 0.19);
  }
  50% {
    transform: rotate(900deg);
    animation-timing-function: cubic-bezier(0.215, 0.61, 0.355, 1);
  }
`;

export const HourglassLoader = styled.div`
  display: flex;
  justify-content: center;
  position: relative;
  width: 100%;
  height: 120px;
  margin: 25px 0;
  text-align: center;

  &:after {
    content: '';
    display: block;
    border-radius: 50%;
    width: 0;
    height: 0;
    margin: 8px;
    box-sizing: border-box;
    border: 32px solid #fff;
    border-color: #f80179 transparent #640eb3 transparent;
    animation: ${hourglassAnimation} 1.2s infinite;
    /*background: linear-gradient(95.08deg, #F80179 2.49%, #640EB3 97.19%);*/
  }
`;

export const SuccessSign = styled.div`
  display: flex;
  justify-content: center;
  position: relative;
  width: 100%;
  height: 120px;
  margin: 25px 0;
  text-align: center;
  font-size: 100px;
  line-height: 100px;
  background-image: -webkit-gradient(
    linear,
    left center,
    right center,
    color-stop(0, #f80179),
    color-stop(1, #bb0691)
  );
  background-image: gradient(
    linear,
    left center,
    right center,
    color-stop(0, #f80179),
    color-stop(1, #bb0691)
  );
  color: transparent;
  -webkit-background-clip: text;
  background-clip: text;
`;
