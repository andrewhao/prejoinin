/* Detect changes in window scroll position and fire updates.
 */
import debounce from 'debounce';

export default class ScrollDetection {
  constructor(window, document, handleScroll) {
    this.document = document;
    this.window = window;
    this.handleScroll = handleScroll
    this.currentScrollPosition = null;
  }

  registerHandler() {
    this.currentScrollPosition = this.window.pageYOffset || this.document.body.scrollTop;
    this.window.onscroll = debounce(() => {
      var newScroll = this.window.pageYOffset || this.document.body.scrollTop;
      this.handleScroll(this.currentScrollPosition, newScroll);
      this.currentScrollPosition = newScroll;
    }, 18);
  };
}
